export interface IChangeTicket {
  tag: "ChangeTicket";
  contents: [TicketID, TicketDiff];
}

export interface ICreateRelationship {
  tag: "CreateRelationship";
  contents: [TicketID, RelationshipType, TicketID];
}

export interface ICreateTags {
  tag: "CreateTags";
  contents: [TicketID, Tag[]];
}

export interface ICreateTicket {
  tag: "CreateTicket";
  contents: [TicketID, Ticket];
}

export interface IFilterByID {
  tag: "FilterByID";
  contents: TicketID;
}

export interface IFilterByName {
  tag: "FilterByName";
  contents: string;
}

export interface IFilterByRelationshipFrom {
  tag: "FilterByRelationshipFrom";
  contents: [RelationshipType, TicketID];
}

export interface IFilterByRelationshipTo {
  tag: "FilterByRelationshipTo";
  contents: [RelationshipType, TicketID];
}

export interface IFilterByStatus {
  tag: "FilterByStatus";
  contents: TicketStatus;
}

export interface IFilterByTag {
  tag: "FilterByTag";
  contents: Tag;
}

export interface IOrderByID {
  tag: "OrderByID";
  contents: OrderingDirection;
}

export interface IOrderByName {
  tag: "OrderByName";
  contents: OrderingDirection;
}

export interface IOrderByStatus {
  tag: "OrderByStatus";
  contents: OrderingDirection;
}

export interface IQuery {
  queryFilters: Filter[];
  queryOrderings: Ordering[];
  queryLimit: Limit | null;
}

export interface IRemoveRelationship {
  tag: "RemoveRelationship";
  contents: [TicketID, RelationshipType, TicketID];
}

export interface IRemoveTags {
  tag: "RemoveTags";
  contents: [TicketID, Tag[]];
}

export interface ITag {
  unTag: string;
}

export interface ITicket {
  name: string;
  description: string;
  status: TicketStatus;
}

export interface ITicketDetails {
  tdTicketID: TicketID;
  tdTicket: Ticket;
  tdTags: Tag[];
  tdRelationships: [RelationshipType, TicketID[]][];
}

export interface ITicketDiff {
  diffName: string | null;
  diffDescription: string | null;
  diffStatus: TicketStatus | null;
}

export interface ITicketID {
  unTicketID: string;
}

export type Command = ICreateTicket | IChangeTicket | ICreateRelationship | ICreateTags | IRemoveTags | IRemoveRelationship;

export type Filter = IFilterByName | IFilterByID | IFilterByTag | IFilterByStatus | IFilterByRelationshipTo | IFilterByRelationshipFrom;

export type ILimit = number;

export type Limit = ILimit;

export type Ordering = IOrderByName | IOrderByID | IOrderByStatus;

export type OrderingDirection = "Ascending" | "Descending";

export type Query = IQuery;

export type RelationshipType = "Blocks" | "Subsumes";

export type Tag = ITag;

export type Ticket = ITicket;

export type TicketDetails = ITicketDetails;

export type TicketDiff = ITicketDiff;

export type TicketID = ITicketID;

export type TicketStatus = "ToDo" | "InProgress" | "InReview" | "Complete" | "WontFix";
