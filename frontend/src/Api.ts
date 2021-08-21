export type TicketDetails = ITicketDetails;

export interface ITicketDetails {
  tdTicketID: TicketID;
  tdTicket: Ticket;
  tdTags: Tag[];
  tdRelationships: [RelationshipType, TicketID[]][];
}

export type Query = IQuery;

export interface IQuery {
  queryFilters: Filter[];
  queryOrderings: Ordering[];
  queryLimit: Limit;
}

export type Command = ICreateTicket | IChangeTicket | ICreateRelationship | ICreateTags | IRemoveTags | IRemoveRelationship;

export interface ICreateTicket {
  tag: "CreateTicket";
  contents: [TicketID, Ticket];
}

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

export interface IRemoveTags {
  tag: "RemoveTags";
  contents: [TicketID, Tag[]];
}

export interface IRemoveRelationship {
  tag: "RemoveRelationship";
  contents: [TicketID, RelationshipType, TicketID];
}

export type RelationshipType = "Blocks" | "Subsumes";

export type TicketID = ITicketID;

export interface ITicketID {
  unTicketID: string;
}

export type Ticket = ITicket;

export interface ITicket {
  name: string;
  description: string;
  status: TicketStatus;
}

export type TicketStatus = "ToDo" | "InProgress" | "InReview" | "Complete" | "WontFix";

export type TicketDiff = ITicketDiff;

export interface ITicketDiff {
  diffName: string | null;
  diffDescription: string | null;
  diffStatus: TicketStatus | null;
}

export type Filter = IFilterByName | IFilterByID | IFilterByTag | IFilterByStatus | IFilterByRelationshipTo | IFilterByRelationshipFrom;

export interface IFilterByName {
  tag: "FilterByName";
  contents: string;
}

export interface IFilterByID {
  tag: "FilterByID";
  contents: TicketID;
}

export interface IFilterByTag {
  tag: "FilterByTag";
  contents: Tag;
}

export interface IFilterByStatus {
  tag: "FilterByStatus";
  contents: TicketStatus;
}

export interface IFilterByRelationshipTo {
  tag: "FilterByRelationshipTo";
  contents: [RelationshipType, TicketID];
}

export interface IFilterByRelationshipFrom {
  tag: "FilterByRelationshipFrom";
  contents: [RelationshipType, TicketID];
}

export type Ordering = "OrderByName" | "OrderByID" | "OrderByStatus";

export type Limit = ILimit;

export type ILimit = number;

export type Tag = ITag;

export interface ITag {
  unTag: string;
}
