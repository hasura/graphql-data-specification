The GraphQL Data Specification (GDS) defines a standardized GraphQL API that
can be exposed on top of various databases. The GraphQL API is defined on top
of an Entity Data Model (EDM).

## Data Model

This section provides a high-level description of the Hasura Data Model (HDM):
the abstract data model which forms the basis for the GraphQL API. The central
concepts in the HDM are entities, relationships and permissions.

### Entity

An 'entity' is where data is typically stored, modified and retrived from a
database. On relational databases, 'tables' and table like objects such as
views and materialized views are entities, on a document database such as
Mongo, 'collections' would be considered as entities. Currently as the spec
focuses only on relational databases, the terms 'entities' and 'tables' are
used interchangeably but this could change in future versions.

We'll assume the following schema for the rest of the sections:

```sql
create table artists (
  id int primary key,
  name text not null
);
create table albums (
  id int primary key,
  name text not null,
  artist_id int not null references artists(id)
);
create table tracks (
  id int primary key,
  name text not null,
  album_id int not null references albums(id),
  milliseconds int not null
);
```

The following operations are defined over an entity:

#### Queries

##### Fetching by primary key

(§query.table.select-by-pk)

If an entity has a primary key defined, this allows fetching a specific row by
providing the primary key of the row:

```graphql
query {
  albums_by_pk(id: 4) {
    id
    title
  }
}
```

would return with

```json
{
  "data": {
    "albums_by_pk": {
      "id": 4,
      "title": "Let There Be Rock"
    }
  }
}
```

##### Generic fetch from an entity

(§query.table.select)

Allows fetching zero or more 'rows' of the table.

```graphql
query {
  track {
    name
  }
}
```

would return all the rows in the `track` table.

```json
{
  "data": {
    "albums": [
      {
        "id": 1,
        "title": "For Those About To Rock We Salute You"
      },
      {
        "id": 3,
        "title": "Restless and Wild"
      },
      ...
    ]
  }
}
```

Additionally,

1. A boolean predicate (§expression.table.filter) can be used to filter the rows.

   ```graphql
   {
    albums(where: {title: {_eq: "Restless and Wild"}}) {
      id
      title
    }
   }
   ```

   would return

   ```json
   {
     "data": {
       "albums": [
         {
           "id": 3,
           "title": "Restless and Wild"
         }
       ]
     }
   }
   ```

2. An 'order by' clause (§expression.table.order_by) can be used to order
   the rows in the result set

   ```graphql
   query {
     albums(order_by: { id: desc}) {
       id
       title
     }
   }
   ```

   would return

   ```json
   {
     "data": {
       "albums": [

         {
           "id": 347,
           "title": "Koyaanisqatsi (Soundtrack from the Motion Picture)"
         },
         {
           "id": 346,
           "title": "Mozart: Chamber Music"
         },
         ...
       ]
     }
   }
   ```

3. 'limit' (§table.expression.limit) and 'offset'
   (§expression.table.offset), togther with 'order_by'
   (§expression.table.order_by) can be used to paginate the result set.

   ```graphql
   query {
     albums(order_by: { id: desc} limit: 1) {
       id
       title
     }
   }
   ```

   would return

   ```json
   {
     "data": {
       "albums": [
         {
           "id": 347,
           "title": "Koyaanisqatsi (Soundtrack from the Motion Picture)"
         }
       ]
     }
   }
   ```

   `offset` can be used to paginate the data:

   ```graphql
   query {
     albums(order_by: { id: desc} limit: 1 offset: 1) {
       id
       title
     }
   }
   ```

   would return

   ```json
   {
     "data": {
       "albums": [
         {
           "id": 346,
           "title": "Mozart: Chamber Music"
         }
       ]
     }
   }
   ```

##### Fetching aggregated data from a table

(§table.query.select-aggregate)

Allows fetching aggregated data from a table. Similar to §query.table.select,
'filter', 'limit' + 'offset' and 'order_by' can be used to target the necessary
rows for aggregation.

```graphql
query {
  albums_aggregate( where: { artist_id: { _eq: 1 } }) {
    aggregate {
      count
    }
  }
}
```

would return

```json
{
  "data": {
    "albums_aggregate": {
      "aggregate": {
        "count": 2
      }
    }
  }
}
```

You can also use aggregation functions that operate on columns. For example,

```graphql
query {
  tracks_aggregate(where: {album_id: {_eq: 1}}) {
    aggregate {
      max { milliseconds }
      min { milliseconds }
      avg { milliseconds }
    }
  }
}
```

would fetch the max, min and sum of 'duration' of each track in the album.

```json
{
  "data": {
    "tracks_aggregate": {
      "aggregate": {
        "max": { "milliseconds": 343719 },
        "min": { "milliseconds": 199836 },
        "avg": { "milliseconds": 240041.5 }
      }
    }
  }
}
```

The rows that were used in the aggregation can also be fetched alongside the
aggregated data:

```graphql
query {
  tracks_aggregate(where: {album_id: {_eq: 1}}) {
    aggregate {
      max { milliseconds }
      min { milliseconds }
      avg { milliseconds }
    }
    nodes {
      name
      milliseconds
    }
  }
}
```

would return

```json
{
  "data": {
    "tracks_aggregate": {
      "aggregate": {
        "max": {
          "milliseconds": 375418
        },
        "min": {
          "milliseconds": 230619
        },
        "avg": {
          "milliseconds": 286029.3333333333
        }
      },
      "nodes": [
        {
          "name": "Fast As a Shark",
          "milliseconds": 230619
        },
        {
          "name": "Restless and Wild",
          "milliseconds": 252051
        },
        {
          "name": "Princess of the Dawn",
          "milliseconds": 375418
        }
      ]
    }
  }
}
```

#### Mutations

##### Inserting a row into a table

(§table.query.insert_one)

Inserts a row in a table. The inserted data can be fetched as part of the
response. This is very useful in cases where you have an auto-generated id.

```graphql
mutation {
  track_insert_one(
    object: {
      title: ...,
      duration_ms: ...
    }
  ) {
    id
  }
}
```

would return:

```json
{
  "id": 121
}
```

##### Insert several rows into a table

(§table.query.insert_many)

Inserts zero or many rows into a table. In addition to the data of the inserted
rows, the numbers of rows that are inserted can also be fetched. Note the
number of inserted rows would typically be the same as the number of rows that
are being inserted, however, it could be different in the presence of triggers
and conflicts.

```graphql
mutation {
  track_insert_many(
    objects: [
      { title: ..., duration_ms: ... },
      { title: ..., duration_ms: ... }
    ]
  ) {
    affected_rows
    returning {
      id
    }
  }
}
```

would return:

```json
{
  "affected_rows": 1,
  "returning": [
    { "id": 122 },
    { "id": 123 }
  ]
}
```

##### Update a row by its primary key

(§mutation.table.update-by-pk)

Allows updating a table's row by using the primary key. The updated data can be
fetched as part of the response.

```graphql
query {
  update_track_by_pk(id: 1 _set: { name: "hello" } ) {
    name
  }
}
```

would return with

```json
{
  "name": "hello"
}
```

`_set` 'operator' allows setting the value of a column. There are other
operators such as `_inc`, `_mul` etc to operate on the existing value of a
column.

```graphql
query {
  update_page_visits_by_pk(id: 1 _inc: { visit_count: 1 } ) {
    visit_count
  }
}
```

would return with

```json
{
  "visit_count": 793
}
```


##### Update several rows by boolean predicate

(§mutation.table.update-many)

Allows updating zero or more rows that match a boolean predicate. The number of
rows that have been updated alongside the updated data can be fetched as part
of the response.

Example: Say we want to cancel all the orders from a particular user.

```graphql
query {
  update_order(
    where: { placed_by: { _eq: 121 } }, _set: { status: "cancelled" }
  ) {
    affected_rows
    returning {
      id
    }
  }
}
```

would return with

```json
{
  "affected_rows": 5,
  "returning": [
    { "id": .. },
    { "id": .. },
    { "id": .. },
    ...
  }
}
```

##### Batch updates across several rows

(§table.mutation.update-batch)

This operation allows batching several update operations into a single
operation. Note that while 'update-many' operation can be used to update
several rows, they are all updated 'similarly'.

Example: Set the status of several orders in a single operation

```graphql
query {
  update_order_bulk(
    operations: [
      { where: {id: { _eq: 121 } }, _set: { status: "cancelled" } },
      { where: {id: { _eq: 139 } }, _set: { status: "shipped" } },
      { where: {id: { _eq: 197 } }, _set: { status: "payment_pending" } },
  ) {
    affected_rows
    returning {
      status
    }
  }
}
```

would return with

```json
{
  "affected_rows": 3,
  "returning": [
    { "status": .. },
    { "status": .. },
    { "status": .. },
  }
}
```

##### Delete a row by its primary key

(§table.mutation.delete-by-pk)

Allows deleting a table's row by using the primary key. The data of the deleted
row can be fetched as part of the response.

```graphql
query {
  delete_track_by_pk(id: 1) {
    name
  }
}
```

would return with

```json
{
  "name": "hello"
}
```

##### Delete rows that match a primary key

(§table.mutation.delete-many)

Allows deleting zero or more rows that match a boolean predicate. The number of
rows that have been deleted alongside the deleted data can be fetched as part
of the response.

Example: Delete all the orders from a particular user.

```graphql
query {
  delete_order(
    where: { placed_by: { _eq: 121 } }
  ) {
    affected_rows
    returning {
      id
    }
  }
}
```

would return with

```json
{
  "affected_rows": 5,
  "returning": [
    { "id": .. },
    { "id": .. },
    { "id": .. },
    ...
  }
}
```

### Relationships

'Relationships' capture the relationships that exist between various entities
in a database which are typically referred to as 'many-to-one', 'one-to-many'
and 'one-to-one' relationships. Relationships augment the querying ability as
follows:

#### Object Relationships

These capture both 'many-to-one' and 'one-to-one' relationships that exist at
the database layer.

The table `track` has a many-to-one relationship to the table `album`, this is
captured as an 'object' relationship at the HDM layer. Similarly, the
many-to-one relationship from `album` to `artist` table can be captured using
an object relationship.

Once an object relationship is defined, the API is augmented as follows:

##### Fetching related data

(§select.table.object-relationship)

Related data can be fetched alongside the data from the table.

Example: When fetching an album's information, the corresponding artist's
information can also be fetched.

```graphql
query {
  albums(where: {id: {_eq: 1}}) {
    title
    artist {
      name
    }
  }
}
```

would return

```json
{
  "data": {
    "albums": [
      {
        "artist": {
          "name": "AC/DC"
        },
        "title": "For Those About To Rock We Salute You"
      }
    ]
  }
}
```

This isn't tied to a single operation, any operation which returns 'track'
data can also return the associated album's data. For example:

```graphql
query {
  albums(where: {id: {_eq: 1}}) {
    title
    artist {
      name
    }
  }
  albums_by_pk(where: {id: {_eq: 1}}) {
    title
    artist {
      name
    }
  }
  albums_aggregate(where: {id: {_eq: 1}}) {
    nodes {
      title
      artist {
        name
      }
    }
  }
}
```

would return the associated `artist` data in all the fields.

##### Filtering using related data

§expression.table.filter.object-relationship: Related data can be used to
filter data from a table.

For example, say you would want to fetch all the albums of the artist
'AC/DC'. Note that the artist's name isn't part of the `album` table but the
related `artist` table.

```graphql
query {
  albums(where: {artist: {name: {_eq: "AC/DC"}}}) {
    title
  }
}
```

would return

```json
{
  "data": {
    "albums": [
      {
        "title": "For Those About To Rock We Salute You"
      },
      {
        "title": "Let There Be Rock"
      }
    ]
  }
}
```

##### Ordering using related data

(§expression.table.order-by.object-relationship)

Related data can be used to order data from a table.

For example, the following query returns albums ordered by the album's artist's
name.

```graphql
query {
  albums(order_by: { artist: { name: asc } }) {
    title
  }
}
```

#### Array Relationships

These capture 'one-to-many' relationships that exist at the database layer.

The table `artist` has a one-to-mony relationship to the table `album`, this is
captured as an 'array' relationship at the HDM layer. Similarly, the
one-to-many relationship from `album` to `track` table can be captured using an
array relationship. Let's call these `albums` and `tracks` respectively.

Once an array relationship is defined, the API is augmented as follows:

##### Fetching related data

1. select.table.array-relationship: Related data can be fetched alongside the
   data from the table.

   Example: When fetching albums data, we can also fetch the tracks of an
   album.

   ```graphql
   query {
     albums(where: {id: {_eq: 1}}) {
       title
       tracks {
         name
       }
     }
   }
   ```

   would return

   ```json
   {
     "data": {
       "albums": [
         {
           "title": "Restless and Wild",
           "tracks": [
             {
               "name": "Fast As a Shark"
             },
             {
               "name": "Restless and Wild"
             },
             {
               "name": "Princess of the Dawn"
             }
           ]
         }
       ]
     }
   }
   ```

   Given that an array relationship returns a list of items, it has the similar
   API as a select statement to filter, order and paginate through them.

   ```graphql
   query {
     albums(where: {id: {_eq: 3}}) {
       title
       tracks (where: {milliseconds: {_gt: 300000}} order_by: { id: asc}) {
         name
       }
     }
   }
   ```

   returns

   ```json
   {
     "data": {
       "albums": [
         {
           "title": "Restless and Wild",
           "tracks": [
             {
               "name": "Princess of the Dawn"
             }
           ]
         }
       ]
     }
   }
   ```

   The following query fetches the longest 3 tracks of all albums:

   ```graphql
   query {
     albums {
       title
       tracks(limit: 3, order_by: {milliseconds: desc}) {
         name
       }
     }
   }
   ```

1. select.table.array-relationship-aggregate: Data of an array relationship can
   also be aggregated and fetched, similar to a table's aggregate. Every array
   relationship has a corresponding `aggregate` field to fetched aggregated
   data.

   For example, the following query fetches all albums and each album's track
   count and total runtime.

   ```graphql
   query {
     albums {
       title
       tracks_aggregate {
         aggregate {
           count
           sum {
             milliseconds
           }
         }
       }
     }
   }
   ```

##### Filtering using related data

1. §expression.table.filter.array-relationship: Related data can be used to
   filter data from a table.

   Example: the following query fetches all the albums which have _at least
   one_ track longer than 500 seconds.

   ```graphql
   query {
     albums(where: {tracks: {milliseconds: {_gt: 5000000}}}) {
       title
     }
   }
   ```

   returns

   ```json
   {
     "data": {
       "albums": [
         {
           "title": "Battlestar Galactica, Season 3"
         },
         {
           "title": "Lost, Season 3"
         }
       ]
     }
   }
   ```

1. §expression.table.filter.array-relationship-aggregate: Aggregated related
   data can be used to filter data from a table.

   Example: The following query fetches all albums who have at least 5 tracks.


   ```graphql
   query {
     albums(where: {tracks_aggregate: {count: {predicate: {_gt: 30}}}}) {
       title
     }
   }
   ```

   would return


   ```json
   {
     "data": {
       "albums": [
         {
           "title": "Minha Historia"
         },
         {
           "title": "Greatest Hits"
         }
       ]
     }
   }
   ```

##### Ordering using related data

Aggregated array relationship data can be used to order data from a table.

For example, the following query fetches the album with the most number of tracks.

```graphql
query {
  albums(order_by: {tracks_aggregate: {count: desc}}, limit: 1) {
    title
  }
}
```

would return

```json
{
  "data": {
    "albums": [
      {
        "title": "Greatest Hits"
      }
    ]
  }
}
```

### Permissions

Permissions are used to restrict access to data stored in entities and is
managed through a system of 'roles'. A 'role' can be a considered a label for a
set of users who have similar access to data.

#### Select Permission

A select permission for a role 'r' on an entity 'e' specifies the data that can
be fetched from the entity by the role. Select permission provides the following
options:

1. `columns`: The columns of the table that are allowed to be selected.
2. `filter`: Only rows matching this predicate are allowed to be fetched from
   the table.
3. `limit`: The number of rows that can be fetched at a time.
4. `allowed_query_root_fields`: The querying capabilities that are exposed.
5. `allowed_subscription_root_fields`:

#### Delete Permission

A delete permission for a role 'r' on a table 't' specifies the data that can
be deleted from the entity by the role. Delete permission provides the following
options:

1. `filter`: Only rows matching this predicate are allowed to be deleted from
   the table.

#### Update  Permission

An update permission for a role 'r' on a table 't' specifies constraints on the
table's data that be updated by the role. Update permission provides the
following options:

1. `columns`: The columns of the table that are allowed to be updated.
1. `presets`: Static or runtime presets for columns.
1. `filter`: Only rows matching this predicate are allowed to be updated from the table.
1. `check`: The condition that the updated row must satisfy.
