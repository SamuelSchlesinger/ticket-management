import React from 'react';
import equal from 'fast-deep-equal';
import './App.css';
import { OrderingDirection, Query, TicketID, RelationshipType, TicketStatus, Ticket, TicketDiff, TicketDetails, Command, Filter, Ordering, Tag } from './Api';
import styled from 'styled-components';

function command(c: Command): Promise<boolean> {
  return fetch('http://localhost:3001/command',
    { method: 'POST',
      mode: 'cors',
      cache: 'no-cache',
      credentials: 'omit',
      headers: {
        'Content-Type': 'application/json'
      },
      redirect: 'follow',
      referrerPolicy: 'no-referrer',
      body: JSON.stringify([c])
    }
  ).then(result => result.ok);
}

function createTicket(ticketID: TicketID, ticket: Ticket): Promise<boolean> {
  return command({ tag: 'CreateTicket', contents: [ticketID, ticket] });
}

function createTags(ticketID: TicketID, tags: Tag[]): Promise<boolean> {
  return command({ tag: 'CreateTags', contents: [ticketID, tags]});
}

function changeTicket(ticketID: TicketID, ticketDiff: TicketDiff): Promise<boolean> {
  return command({ tag: 'ChangeTicket', contents: [ticketID, ticketDiff] });
}

function createRelationship(ticketID: TicketID, relationshipType: RelationshipType, targetID: TicketID): Promise<boolean> {
  return command({ tag: 'CreateRelationship', contents: [ticketID, relationshipType, targetID] });
}

function removeTags(ticketID: TicketID, tags: Tag[]) {
  return command({ tag: 'RemoveTags', contents: [ticketID, tags] });
}

function removeRelationship(ticketID: TicketID, relationshipType: RelationshipType, targetID: TicketID): Promise<boolean> {
  return command({ tag: 'RemoveRelationship', contents: [ticketID, relationshipType, targetID] });
}

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

function ifThenElse<X>(cond: boolean, x1: X, x2: X): X {
  if (cond) {
    return x1;
  } else {
    return x2;
  }
}

const Menu = styled.div`
  overflow: hidden;
  text-align: left;
  clear: left;
  float: left;
`

const FilterList = styled.ul`
  list-style-type: none;
  overflow: auto;
  padding: 0;
  margin: 0;
  float: left;
`

const DisplayFilter = styled.li`
  float: left;
  text-align: center;
  padding: 4px;
  text-decoration: italic;
  background: white;
`

const DeleteFilterButton = styled.button`
  margin: 4px;
  background: white;
`

function FilterMenu(props: { query: Query, setQuery: React.Dispatch<React.SetStateAction<Query>> }) {
  const [inputText, setInputText] = React.useState("");
  const [filterType, setFilterType] = React.useState<FilterType>("name");
  const handleFilterTypeChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    setFilterType(event.target.value as FilterType);
  };
  const handleInputTextChange = (event: React.ChangeEvent<HTMLInputElement>) =>
    setInputText(event.target.value);
  const handleInputTextChangeStatus = (event: React.ChangeEvent<HTMLSelectElement>) =>
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
    <Menu className="FilterMenu">
      <section id="filter-type-input">
        <label>
          <p className="menu-label"> Filter </p>
          <select name="filter-type" className="menu-input" id="filter-type" onChange={handleFilterTypeChange}>
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
            { ifThenElse(filterType === "status",
              <select name="status" className="menu-input" id="status" onChange={handleInputTextChangeStatus}>
                <option value="ToDo"> To Do </option>
                <option value="InProgress"> In Progress </option>
                <option value="Complete"> Complete </option>
                <option value="WontFix"> Won't Fix </option>
              </select>
              ,
              <input
                type="text"
		className="menu-input"
                onChange={handleInputTextChange}
                value={inputText}
              />
              )
            }
          </label>
          <button type="button" className="menu-input" onClick={handleSubmit}> Filter </button>
        </form>
      </section>
      <section>
        <FilterList id="filters">
          {props.query.queryFilters.map((item) => (
            <DisplayFilter key={JSON.stringify(item)}>
              {renderFilter(item)}
              <DeleteFilterButton onClick={handleDeleteItem(item)} className="menu-input">×</DeleteFilterButton>
            </DisplayFilter>
          ))}
        </FilterList>
      </section>
    </Menu>
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
    <Menu className="LimitMenu">
      <form onSubmit={handleSubmit}>
        <label>
          <p className="menu-label"> Limit </p>
	  <input type="number" className="menu-input" id="limit" name="limit" min="1" onChange={handleLimitChange} />
        </label>
      </form>
    </Menu>
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
    <Menu className="OrderingMenu">
      <form>
        <label>
          <p className="menu-label"> Ordering </p>
          <select name="ordering" className="menu-input" id="ordering" onChange={handleOrderingChange}>
            <option value="id"> ID </option>
            <option value="name"> Name </option>
            <option value="status"> Status </option>
          </select>
          <select name="direction" className="menu-input" id="direction" onChange={handleDirectionChange}>
            <option value="ascending"> Ascending </option>
            <option value="descending"> Descending </option>
          </select>
        </label>
        <button type="button" className="menu-input" onClick={handleChange}> Sort </button>
      </form>
    </Menu>
  );
}

const EditButton = styled.button`
  padding: 2px;
  margin: 2px;
`

function TicketDetailsView(props: { td: TicketDetails, setQuery: React.Dispatch<React.SetStateAction<Query>>}) {
  const [ticketDiff, setTicketDiff] = React.useState<TicketDiff>({diffName: null, diffDescription: null, diffStatus: null})
  const [newTag, setNewTag] = React.useState<Tag | null>(null);
  const seeOneTicket = (tid:TicketID) => (_e: React.MouseEvent<HTMLButtonElement>) => {
    props.setQuery((q) => {
      return {...q, queryFilters: [{tag: "FilterByID", contents: tid}]};
    }
    );
  }
  const seeTagged = (t:Tag) => (_e: React.MouseEvent<HTMLButtonElement>) => {
    props.setQuery((q) => {
      return {...q, queryFilters: [{tag: "FilterByTag", contents: t}]};
    }
    );
  }
  const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setTicketDiff((d) => { return {...d, diffName: e.target.value} });
  }
  const setDescription = (x: string) => {
    setTicketDiff((d) => { return { ... d, diffDescription: x } });
  }
  const setName = (x: string) => {
    setTicketDiff((d) => { return { ... d, diffName: x } });
  }
  const setStatus = (x: TicketStatus) => {
    setTicketDiff((d) => { return { ... d, diffStatus: x } });
  }
  const handleStatusChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    setTicketDiff((d) => { return {...d, diffStatus: e.target.value as TicketStatus} });
  }
  const handleDescriptionChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setTicketDiff((d) => { return {...d, diffDescription: e.target.value} });
  }

  const handleNewTagChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setNewTag((t) => { return { unTag: e.target.value } });
  }

  const nullDiffName = (d: TicketDiff) => {
    return {...d, diffName: null };
  }

  const nullDiffDescription= (d: TicketDiff) => {
    return {...d, diffDescription: null };
  }

  const nullDiffStatus = (d: TicketDiff) => {
    return {...d, diffStatus: null };
  }

  const onSubmitTag = (_e: React.FormEvent) => {
    createTags(props.td.tdTicketID, [newTag as Tag]).then((result) => {
      if (result) {
        props.setQuery((q) => { return {...q}});
        setNewTag((t) => null);
      }
    });
  }

  const onSubmitDiff = (f: (x: TicketDiff) => TicketDiff) => (_e: React.FormEvent) => {
    changeTicket(props.td.tdTicketID, ticketDiff).then((result) =>
      {
        if (result) {
          // Successfully changed the ticket
          setTicketDiff((d) => f(d));
            props.setQuery((q) => { return {...q} });
        }
      }
    );
  }
  return (
    <dl className="TicketDetails">
    <dt>Ticket ID</dt>
    <dd> "{ props.td.tdTicketID.unTicketID }" </dd>
    <dt>Name</dt>
    <dd>
    {
      ifThenElse(ticketDiff.diffName === null
      , <p> "{ props.td.tdTicket.name }" <EditButton onClick={() => setName(props.td.tdTicket.name)}>Edit</EditButton></p>
      , <form >
          <input type="text" onChange={handleNameChange} />
          <button onClick={onSubmitDiff(nullDiffName)}> ✓ </button>
          <button type="button" onClick={e => setTicketDiff((d) => { return {...d, diffName: null} } )}> ✗ </button>
        </form>
      )
    }
    </dd>
    <dt>Status</dt>
    <dd>
    {
      ifThenElse(ticketDiff.diffStatus === null
      , <p> { props.td.tdTicket.status } <EditButton onClick={() => setStatus(props.td.tdTicket.status)}>Edit</EditButton></p>
      , <form onSubmit={onSubmitDiff(nullDiffStatus)}>
          <select onChange={handleStatusChange} value={props.td.tdTicket.status}>
            <option value="ToDo"> To Do </option>
            <option value="InProgress"> In Progress</option>
            <option value="InReview"> In Review </option>
            <option value="Complete"> Complete </option>
            <option value="WontFix"> Won't Fix </option>
          </select>
          <button onClick={onSubmitDiff(nullDiffStatus)}> ✓ </button>
          <button type="button" onClick={e => setTicketDiff((d) => { return {...d, diffStatus: null} } )}> x </button>
        </form>
      )
    }
    </dd>
    <dt>Description</dt>
    <dd>
    {
      ifThenElse(ticketDiff.diffDescription === null
      , <p> { props.td.tdTicket.description } <EditButton onClick={() => setDescription(props.td.tdTicket.description)}>Edit</EditButton></p>
      , <form >
          <input type="text" size={80} onChange={handleDescriptionChange} />
          <button onClick={onSubmitDiff(nullDiffDescription)}> ✓ </button>
          <button type="button" onClick={e => setTicketDiff(nullDiffDescription)}> ✗ </button>
        </form>
      )
    }
    </dd>
    <dt>Tags</dt>
    <dd>
      <ul className="comma-separated-list">
        <li>
          <ul className="comma-separated-list">
            { props.td.tdTags.map((tag) =>
                <li> <button onClick={seeTagged(tag)}> { tag.unTag } </button> </li>
              )
            }
          </ul>
        </li>
        <li>
          { ifThenElse (newTag === null,
              <EditButton type="button" onClick={() => setNewTag((c) => { return { unTag: ""} })}> + </EditButton>
            , <form>
                <input type="text" size={20} onChange={handleNewTagChange}/>
                <button onClick={onSubmitTag}> ✓ </button>
                <button type="button" onClick={e => setNewTag((t) => null)}> ✗ </button>
              </form>
            )
          }
        </li>
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
                  <li> <button onClick={seeOneTicket(ticketID)}> "{ ticketID.unTicketID }" </button> </li>
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

const CreateForm = styled.div`
  clear: left;
`

const NewButton = styled.button`
  padding: 1%;
  clear: all;
  margin: auto;
`

function NewTicketMenu(props: { query: Query, setQuery: React.Dispatch<React.SetStateAction<Query>> }) {
  const [newTicket, setNewTicket] = React.useState<[TicketID, Ticket] | null>(null);
  const init = (newTick: [TicketID, Ticket] | null): [TicketID, Ticket] => {
    if (newTick === null) {
      return [ { unTicketID: "" }, { name: "", description: "", status: 'ToDo' }];
    } else {
      return newTick;
    }
  }
  const onSubmit = (e: React.FormEvent) => {
    const [ticketID, ticket] = init(newTicket)
    createTicket(ticketID, ticket).then(result => {
      if (result) {
        setNewTicket((t) => null);
      }
      }
    );
    setNewTicket(null);
    props.setQuery((q) => {return {...q}});
  }
  const startEditingTicket = (event: React.MouseEvent<HTMLButtonElement>) => {
    setNewTicket((_t) =>
      {
        return [ { unTicketID: "" }, { name: "", description: "", status: 'ToDo' } ]
      }
    );
    }
  const onChangeName = (event: React.ChangeEvent<HTMLInputElement>) => {
    setNewTicket((t) =>
      {
        const t_ = init(t);
        return [ t_[0], { ... t_[1], name: event.target.value } ]; 
      }
    );
  }
  const onChangeDescription = (event: React.ChangeEvent<HTMLInputElement>) => {
    setNewTicket((t) =>
      {
        const t_ = init(t);
        return [ t_[0], { ... t_[1], description: event.target.value } ];
      }
    );
  }
  const onChangeID = (event: React.ChangeEvent<HTMLInputElement>) => {
    setNewTicket((t) => {
      const t_ = init(t);
      return [ { unTicketID: event.target.value }, t_[1] ];
    });
  }
  const onChangeStatus = (event: React.ChangeEvent<HTMLSelectElement>) => {
    setNewTicket((t) =>
      {
        const t_ = init(t);
        return [ t_[0], { ... t_[1], status: event.target.value as TicketStatus } ];
      }
    );
  }
  const nullNewTicket = (event: React.MouseEvent<HTMLButtonElement>) => {
    setNewTicket((_t) => null);
  }
  return (
    <CreateForm>
      { ifThenElse(newTicket === null,
          <NewButton type="button"
            onClick={startEditingTicket}> Create New Ticket </NewButton>
        , <form onSubmit={onSubmit}>
            <label>
            ID
            <input name="id" type="text" onChange={onChangeID}/>
            </label>
            <label>
            Name
            <input name="name" type="text" onChange={onChangeName}/>
            </label>
            <label>
            Description
            <input name="description" type="text" onChange={onChangeDescription}/>
            </label>

            <label>
            Status
              <select onChange={onChangeStatus}>
                <option value="ToDo">To Do</option>
                <option value="InProgress">In Progress </option>
                <option value="InReview"> In Review </option>
                <option value="Complete"> Completed </option>
                <option value="WontFix"> Won't Fix </option>
              </select>
            </label>
            <button onClick={onSubmit}> ✓ </button>
            <button type="button" onClick={nullNewTicket}> x </button>
          </form>
        )
      }
    </CreateForm>
  );
}

function Tickets() {
  const [q, setQuery] = React.useState<Query>({ queryFilters: [], queryOrderings: [{tag: "OrderByID", contents: "Ascending"}], queryLimit: null });
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
        <br/>
        <NewTicketMenu query={q} setQuery={setQuery}/>
      </section>
      <section>
        <ul id="tickets">
          { tickets.map(td =>
            { return ( <li>
                       <TicketDetailsView td={td} setQuery={setQuery} key={td.tdTicketID.unTicketID} />
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
