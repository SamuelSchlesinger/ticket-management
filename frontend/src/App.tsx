import React from 'react';
import equal from 'fast-deep-equal';
import './App.css';
import { Query, TicketDetails, Filter, Ordering, TicketStatus } from './Api';

function query(q : Query): Promise<TicketDetails[]> {
  return fetch('http://localhost:3001/query', { method: 'POST',
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
  const [filters, setFilters] = React.useState<Filter[]>([]);
  const [orders, setOrders] = React.useState<Ordering[]>([]);
  const [limit, setLimit] = React.useState<number>(100);
  const [tickets, setTickets] = React.useState<TicketDetails[]>([]);
  React.useEffect(() =>
    { const setEm = async () =>
      { const ts = await query({queryFilters: filters, queryOrderings: orders, queryLimit: limit});
        setTickets(ts);
      };
      setEm();
    }
    , []);
  return (
    <div className="App">
      <header className="App-header">
        <h1> Ticket System </h1>
        <FilterMenu filters={filters} setFilters={setFilters}/>
        <Tickets tickets={tickets}/>
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
      const f = (s: TicketStatus) => { return { tag: "FilterByStatus", contents: s } }
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

function FilterMenu(props: { filters: Filter[], setFilters: React.Dispatch<React.SetStateAction<Filter[]>> }) {
  const [inputText, setInputText] = React.useState("");
  const [filterType, setFilterType] = React.useState<FilterType>("name");
  const handleFilterTypeChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    setFilterType(event.target.value as FilterType);
  };
  const handleInputTextChange = (event: React.ChangeEvent<HTMLInputElement>) =>
    setInputText(event.target.value);
  const handleDeleteItem = (item: Filter) => (
    _event: React.MouseEvent<HTMLButtonElement>
  ) => props.setFilters((fs) => fs.filter((item_) => {
      return !equal(item_, item);
    })
  );
  const handleSubmit = (event: React.FormEvent) => {
    event.preventDefault();
    props.setFilters((fs) => {
        const f = mkFilter(filterType, inputText);
        if (fs.every((item) => !equal(item, f))) { 
          return fs.concat(f);
        } else {
          return fs;
        }
      }
    );
    setInputText("");
  };

  return (
    <div className="FilterMenu">
      <section>
        <label>
          Choose Filter Type
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
      <section>
        <form onSubmit={handleSubmit}>
          <label>
            Enter a Filter
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
          {props.filters.map((item) => (
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
    </dl>
  );
}

function Tickets(props: { tickets: [TicketDetails]}) {
  return (
    <ul id="tickets">
      { props.tickets.map(td =>
        { return ( <li>
                   <TicketDetailsView td={td} key={td.tdTicketID.unTicketID} />
                   </li>
          );
        }
      )
      }
    </ul>
  );
  }
