import React from 'react';
import equal from 'fast-deep-equal';
import './App.css';
import { OrderingDirection, Query, TicketDetails, Filter, Ordering } from './Api';

function query(q : Query): Promise<TicketDetails[]> {
  return fetch('http://localhost:3001/query',
    { method: 'POST',
      mode: 'cors', // no-cors, *cors, same-origin
      cache: 'no-cache', // *default, no-cache, reload, force-cache, only-if-cached
      credentials: 'omit', // include, *same-origin, omit
      headers: {
        'Content-Type': 'application/json'
      },
      redirect: 'follow', // manual, *follow, error
      referrerPolicy: 'no-referrer', // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
      body: JSON.stringify(q) // body data type must match "Content-Type" header
      }
  ).then(result => result.json());
} 

export function App() {
  return (
    <div className="App">
      <header className="App-header">
        <h1> Ticket System </h1>
        <Tickets />
      </header>
    </div>
  );
}

type FilterType = "name" | "tag" | "id" | "status" | "blocks" | "blocked-by" | "subsumes" | "subsumed-by"

function mkFilter(filterType: FilterType, inputText: string): Filter {
  switch (filterType) {
    case "name":
      return { tag: "FilterByName", contents: inputText };
    case "tag":
      return { tag: "FilterByTag", contents: { unTag: inputText } }; 
    case "id":
      return { tag: "FilterByID", contents: { unTicketID: inputText } };
    case "status":
      switch (inputText) {
        case "ToDo":
          return { tag: "FilterByStatus", contents: "ToDo" };
        case "InProgress":
          return { tag: "FilterByStatus", contents: "InProgress"};
        case "InReview":
          return { tag: "FilterByStatus", contents: "InReview"};
        case "Complete":
          return { tag: "FilterByStatus", contents: "Complete"};
        case "WontFix":
          return { tag: "FilterByStatus", contents: "WontFix"};
	default:
	  throw new Error("Please input correct status");
      }
    case "blocks":
      return { tag: "FilterByRelationshipTo", contents: ["Blocks", { unTicketID: inputText }] };
    case "blocked-by":
      return { tag: "FilterByRelationshipFrom", contents: ["Blocks", { unTicketID: inputText }] };
    case "subsumes":
      return { tag: "FilterByRelationshipTo", contents: ["Subsumes", { unTicketID: inputText }] };
    case "subsumed-by":
      return { tag: "FilterByRelationshipFrom", contents: ["Subsumes", { unTicketID: inputText }] };
    default:
      throw new Error("Please input correct status");
  }
}

function renderFilter(f: Filter): string {
  switch (f.tag) {
    case "FilterByName":
      return "name: " + f.contents;
    case "FilterByID":
      return "id: " + f.contents.unTicketID;
    case "FilterByTag":
      return "tag: " + f.contents.unTag;
    case "FilterByStatus":
      return "status: " + f.contents;
    case "FilterByRelationshipTo":
      switch (f.contents[0]) {
        case "Blocks":
          return "blocks: " + f.contents[1].unTicketID;
        case "Subsumes":
          return "subsumes: " + f.contents[1].unTicketID;
	default:
	  throw new Error("Impossible");
      }
    case "FilterByRelationshipFrom":
      switch (f.contents[0]) {
        case "Blocks":
          return "blocked by: " + f.contents[1].unTicketID;
        case "Subsumes":
          return "subsumed by: " + f.contents[1].unTicketID;
	default:
	  throw new Error("Impossible");
      }
    default:
      throw new Error("Impossible");
  }
}

function FilterMenu(props: { query: Query, setQuery: React.Dispatch<React.SetStateAction<Query>> }) {
  const [inputText, setInputText] = React.useState("");
  const [filterType, setFilterType] = React.useState<FilterType>("name");
  const handleFilterTypeChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    setFilterType(event.target.value as FilterType);
  };
  const handleInputTextChange = (event: React.ChangeEvent<HTMLInputElement>) =>
    setInputText(event.target.value);
  const handleDeleteItem = (item: Filter) => (
    _event: React.MouseEvent<HTMLButtonElement>
  ) => props.setQuery((q) => { return { ...q, queryFilters: q.queryFilters.filter((item_) => !equal(item_, item)) }; });
  const handleSubmit = (event: React.FormEvent) => {
    event.preventDefault();
    props.setQuery((q) => {
        const f = mkFilter(filterType, inputText);
        if (q.queryFilters.every((item) => !equal(item, f))) { 
          return {...q, queryFilters: props.query.queryFilters.concat(f)};
        } else {
          return q;
        }
      }
    );
    setInputText("");
  };
  return (
    <div className="FilterMenu">
      <section id="filter-type-input">
        <label>
          <p className="menu-label"> Filter </p>
          <select name="filter-type" id="filter-type" onChange={handleFilterTypeChange}>
            <option value="name"> Name </option>
            <option value="tag"> Tag </option>
            <option value="id"> ID </option>
            <option value="status"> Status </option>
            <option value="blocks"> Blocks </option>
            <option value="blocked-by"> Blocked By </option>
            <option value="subsumes"> Subsumes </option>
            <option value="subsumed-by"> Subsumed By </option>
          </select>
        </label>
      </section>
      <section id="filter-input">
        <form onSubmit={handleSubmit}>
          <label>
            <input
              type="text"
              onChange={handleInputTextChange}
              value={inputText}
            />
          </label>
        </form>
      </section>
      <section>
        <ul id="filters">
          {props.query.queryFilters.map((item) => (
            <li key={JSON.stringify(item)}>
              {renderFilter(item)}
              <button onClick={handleDeleteItem(item)}>×</button>
            </li>
          ))}
        </ul>
      </section>
    </div>
  );
}

function LimitMenu(props: { query: Query, setQuery: React.Dispatch<React.SetStateAction<Query>> }) {
  const [inputText, setInputText] = React.useState<string>("");
  const handleSubmit = (event: React.FormEvent) => {
    event.preventDefault();
    props.setQuery((q) => {
        const l = parseInt(inputText, 10);
	return {...q, queryLimit: l};
      }
    );
    setInputText("");
  };
  const handleLimitChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setInputText(event.target.value);
  };
  return (
    <div className="LimitMenu">
      <form onSubmit={handleSubmit}>
        <label>
          <p className="menu-label"> Limit </p>
	  <input type="number" id="limit" name="limit" min="1" onChange={handleLimitChange} />
        </label>
      </form>
    </div>
  );
}

function mkOrdering(t: string, d: string): Ordering {
  const d_: () => OrderingDirection = () => {
    if (d === "ascending") {
      return "Ascending";
    } else if (d === "descending") {
      return "Descending";
    } else {
      throw new Error("Impossible");
    }
  };
  if (t === "id") {
    return { tag: "OrderByID", contents: d_()};
  } else if (t === "name") {
    return { tag: "OrderByName", contents: d_() };
  } else if (t === "status") {
    return { tag: "OrderByStatus", contents: d_()};
  } else {
    throw new Error("Impossible");
  }
}

function OrderingMenu(props: { query: Query, setQuery: React.Dispatch<React.SetStateAction<Query>> }) {
  const [ordering, setOrdering] = React.useState<string>("id");
  const [direction, setDirection] = React.useState<string>("ascending");
  const handleChange = (_event: React.MouseEvent<HTMLButtonElement>) => {
    props.setQuery((q) => {
        const o = mkOrdering(ordering, direction);
        console.log(JSON.stringify(o));
        return {...q, queryOrderings: [o]};
      }
    );
  };
  const handleOrderingChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    setOrdering(event.target.value);
  };
  const handleDirectionChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    setDirection(event.target.value);
  };
  return (
    <div className="OrderingMenu">
      <form>
        <label>
          <p className="menu-label"> Ordering </p>
          <select name="ordering" id="ordering" onChange={handleOrderingChange}>
            <option value="id"> ID </option>
            <option value="name"> Name </option>
            <option value="status"> Status </option>
          </select>
          <select name="direction" id="direction" onChange={handleDirectionChange}>
            <option value="ascending"> Ascending </option>
            <option value="descending"> Descending </option>
          </select>
        </label>
        <button type="button" onClick={handleChange}> Sort </button>
      </form>
    </div>
  );
}

function TicketDetailsView(props: { td: TicketDetails }) {
  return (
    <dl className="TicketDetails">
    <dt>Ticket ID</dt>
    <dd>"{props.td.tdTicketID.unTicketID}"</dd>
    <dt>Name</dt>
    <dd>{props.td.tdTicket.name}</dd>
    <dt>Status</dt>
    <dd>{props.td.tdTicket.status}</dd>
    <dt>Description</dt>
    <dd>{props.td.tdTicket.description}</dd>
    <dt>Tags</dt>
    <dd>
      <ul className="comma-separated-list">
        { props.td.tdTags.map((tag) =>
            <li> { tag.unTag } </li>
          )
        }
      </ul>
    </dd>
    <dt>Relationships</dt>
    <dd>
      { props.td.tdRelationships.map(([rel, tickets]) =>
          <ul className="colon-separated-list">
          <li className="embolden"> { rel } </li>
          <li>
            <ul className="comma-separated-list">
              { tickets.map((ticketID) =>
                  <li> "{ ticketID.unTicketID }" </li>
                )
              }
            </ul>
          </li>
          </ul>
        )
      }
    </dd>
    </dl>
  );
}

function Tickets() {
  const [q, setQuery] = React.useState<Query>({ queryFilters: [], queryOrderings: [], queryLimit: null });
  const [ tickets, setTickets ] = React.useState<TicketDetails[]>([]) 
  React.useEffect(() =>
    { const setEm = async () =>
      { const ts = await query(q);
        setTickets(ts);
      };
      setEm();
    }
    , [q]);
  return (
    <div className="Tickets">
      <section>
        <FilterMenu query={q} setQuery={setQuery}/>
        <OrderingMenu query={q} setQuery={setQuery}/>
        <LimitMenu query={q} setQuery={setQuery}/>
      </section>
      <section>
        <ul id="tickets">
          { tickets.map(td =>
            { return ( <li>
                       <TicketDetailsView td={td} key={td.tdTicketID.unTicketID} />
                       </li>
              );
            }
          )
          }
        </ul>
      </section>
    </div>
  );
  }