Navigating Associations with OGQL
---------------------------------

This specification describes an Object Graph Qualification Language.
This *mini-language* allows you to query The Repository and produce a complex
object graph from a subset of the associations stored in the repository. The
language is defined to make it easy to embed queries into a URL.

Specification
=============

The document describes the (informal) specification for Object Graph
Qualification Language (OGQL). OGQL is effectively a search extension that
provides a neat syntax for constructing an object graph based on simple conditions
describing the graph edges you wish to traverse, along with a series of filters
that can be applied to the vertices (instance data nodes) of each edge.

### A Language for Graph Traversal

The basic purpose of the API is to provide a high level language that describes
a graph traversal. Predicates are applied to both the vertices and edges of the
graph, controlling the paths that will be selected.

The result of an OGQL query can be returned in various ways, but the primary type
of result set is an [adjacency list](http://en.wikipedia.org/wiki/Adjacency_list)
annotated with the path for each record (represented as a delimited string of
object ids) and the distance from the starting point. The ordering of the records
is *depth first* by default, which actually means *ordered by path and distance*
in practise. This CAN be overridden if required, as described later in this
document.

Alternative representations (such as pivoted and hierarchical result sets) are
provided as recommendations
[elsewhere](https://github.com/nebularis/ogql).

#### Traversal Rules

The rules governing traversal are fairly simple. Each edge must be directly
connected to its immediate ancestor via the providing end of the association
(i.e., the right node of the previous step must match the left node of the
following). For any edge selected, both nodes MUST satisfy ALL join conditions
declared in the *step* for the edge to be included in the result set.

#### Basic Syntax

The basic syntax of an OGQL query is expressed by the following PEG grammar:

```
query                       <- (intersection / union / set)+ subquery?;
set                         <- (negated_traversal_operator / 
                                recursion_operator)? (name_predicate / 
                                                      type_predicate /
                                                      filter_predicate /
                                                      group) ;
intersection                <- set intersect_operator query ;
union                       <- set union_operator query ;
subquery                    <- subquery_operator query ;
group                       <- ('(' (!')' query)* ')') ;
name_predicate              <- (![A-Z] word)+ bracketed_expression? ;
type_predicate              <- ('?' / (![a-z] word)+)+ bracketed_expression? ;
axis_predicate              <- (&'^' (![A-Z] word))+ ;
filter_predicate            <- (&name_predicate bracketed_expression) /
                                (&type_predicate bracketed_expression) ;
bracketed_expression        <- ('[' (!']' expression_list)* ']') ;
expression_list             <- head:expression tail:(junction expression)* ;
junction                    <- conjunction / disjunction ;
conjunction                 <- space+ 'AND' space+ ;
disjunction                 <- space+ 'OR'  space+ ;
expression                  <- data_point space? operator space? data_point_or_literal ;
data_point_or_literal       <- data_point / literal ;
data_point                  <- axis? '::' member_name ;
axis                        <- normative_axis / backreference '.' normative_axis ;
normative_axis              <- 'provider' / 'consumer' / 'left' / 'right' ;
backreference               <- (&'@' index)+ ;
member_name                 <- '$(' (!')' .)+ ')' / (![A-Z] word)+ / axis_predicate ;
operator                    <- '@' / 'eq' / 'gt' / 'lt' / 'neq' / 'lteq' / 'gteq' /
                                 'like' / 'matches' / 'contains' / 'in' /
                                 'starts_with' / 'ends_with' / 'path_exists' ;
literal                     <- literal_string / literal_number / date /
                                 version / boolean / constant ;
literal_string              <- "'" (!"'" .)* "'" ;
literal_number              <- literal_float / literal_int ;
literal_float               <- [0-9]+ '.' [0-9]+ ;
literal_int                 <- [0-9]+ ;
date                        <- 'DATE(' (!')' .)* ')' ;
version                     <- 'VSN(' (!')' .)* ')' ;
boolean                     <- 'TRUE' / 'FALSE' ;
constant                    <- ":" word+ ;
index                       <- [0-9]+ ;
%asset_name                  <- [\\w_]+ ;
%semver                      <- ([0-9]+ '.' [0-9]+ '.' [0-9]+) ;
word                        <- [\\w\\-_]+ ;
space                       <- [ \\t\\n\\s\\r] / crlf ;
crlf                        <- [\\r]? [\\n] ;
%sep                         <- (space+)? traversal_operator (space+)? ;
intersect_operator          <- (space+)? '=>' (space+)? ;
union_operator              <- (space+)? ',' (space+)? ;
subquery_operator           <- (space+)? '<-' (space+)? ;
negated_traversal_operator  <- '!' ;
recursion_operator          <- '*' ;
```

### Structure, Traversal and Joining

An OGQL query is broken up into a set of ordered *steps*, defining how edges are
to be connected. There are two primary kinds of step (to be discussed shortly),
which can be extended with additional constraints to increase the selectivity
of the edges chosen when producing the result set.

Steps are wired together using *join operators*, which define the relationship
between edges.

The three *join* operators supported are

1. intersection         - written as `a => b`
2. union                - written as `a, b`
3. relative complement  - written as `a | b`

All the *join operators* are right associative. When `intersection` takes place,
The parent/provider vertex (node) of each edge MUST be a child/consumer vertex
(node) from the output of the previous step in order to qualify. 

To make this clear, we can look at the following graph, represented as a list
of edges (x, y):

    [(b, d), (d, z), (c, e), (a, b), (b, c), (e, f), (n, k), (k, v)]

The adjacency list for this graph, starting at `a` would look like this:

    distance      path
    ---------------------
    1             a => b
    2             b => c
    3             c => e
    4             e => f
    2             b => d
    3             d => z

Note that the edges `(n, k), (k, v)` are *not* included in the results, because
they are not found anywhere on the path from the starting point `a`. Understanding
this constraint is vital.
----
The `union` operator, on the other hand, produces the (distinct) set of edges
from two steps. In the case `a, b`, all a's and all b's will be returned (as a
distinct set) in a flat adjacency list. A `union` is mostly useful as a
consuming step in an `intersection`, for example

    a-b => (b-c, b-d)

In this case, the *b's* in the `union` are candidates for intersecting with the
*b* from the *right nodes* of the preceding step in the `intersection`. 

Another important aspect of `union` is that *all* the *right nodes* in the set
are potential inputs to any subsequent steps. We could thus produce two 
distinct paths through the previous graph like so:

    a-b => b-c, b-d => c-e, d-z

In both the previous examples, we can introduce parenthesis as a form of
grouping, although this isn't required because of the relative fixity of these
operations. For clarity, the query above can be rewritten as

    a-b => ((b-c, b-d) => (c-e, d-z))

----
The `relative complement` operation simply acts as a filter, producing all the
edges in the preceding step whose *right nodes* __do not__ match any *left node*
in the following step. Because of the associativity rules, these filtered edges
become the inputs to the next step, so that we can construct a query such as
this (with parenthesis given for readability):

    ((a-b | b-c) => (b-d => d-z))

This effect can also be achieved using the various kinds of predicate available
in the query, which will now be discussed in more detail.

#### The Implicit Name Predicate

The first and most important step predicate is the *implicit name predicate* which
is used to filter the edges before further filtering is applied to its constituent
vertices. This predicate is applied by gathering only associations (the edges of
our graph) whose association type name (see [[MODEL]]) matches the specified
string. For example, if we are traversing the following model:

                  			(stockSupplier)
	    +----------+ Stock +---------------+ Supplier
	    |                                       |
	    | (whstock)                             | (supContract)
	    +             (whcontract)              +
	Warehouse +----------------------------+ Contract


We might issue an OGQL query such as `whstock` which would return all warehouses
and all their associated stocks, because the association type between these two
is named *whstock* and that was the (implicit) predicate we applied to the query.

Another example would be traversing from the `Warehouse` via a `Stock` to a
specific `Supplier`, which would be achieved with the query
`whstock => stockSupplier`.
In this case, the edges identified in the first step would be used to filter over
all the *stockSupplier* edges based on the `Stock` nodes themselves, guaranteeing
that you only link to suppliers whose `Stock` you currently hold.

#### Type Name Predicates

Whereas an *implicit name predicate* filters on a *named association* - which
we might think of as a kind of *tagged edge* in our graph - a predicate on a
*Type Name* filters by *Type*. Clearly this concept is going to be implementation
dependant, so that for a language with static typing (such as C++, Java, .NET and
the like) the *Type Name* might be an indication of the classifier for the type
hierarchy to which the vertices belong. A database driven implementation might
equally use this to indicate a table (or noSQL bucket) name, or even a type
indicator column in an EAV schema.

The primary rule governing *Type Name Predicates* is thus; When the predicate
occurs in the initial step of a query, it indicates the type of the providing
vertices in the first set of edges produced (i.e., the *right nodes*). In all
other positions within the query, the predicate indicates the type of any
consuming vertex (i.e., the *left nodes*). By way of example then, in the
following query, the initial predicate acts as a filter on the edges produced
by the second (following) step. In contrast, the final predicate indicates that
*any* edge containing *left nodes* of the given type, should be returned,
regardless of the type of edge it is (i.e., without reference to the *tagging*
of edges that we see with *implicit name predicates*). Obviously the rules 
governing the use of the `intersection` operator still apply, such that only
edges with inputs matching the outputs of the previous step(s) are valid:

    Person => person-customer => Account

#### Filter Predicates

A filter predicate immediately follows either an *implicit name predicate* or
a *type name predicate*. It is an expression enclosed in square brackets.

Access to the nodes (connected by an association) is achieved by specifying
the axes on which the node lies. The two axes which MUST be supported are *parent*
and *child* which refer to the left and right nodes of the association
respectively. Implementations MAY alias these axes using the terms *provider* (for
parent) and *consumer* (for child). Attributes of the edge nodes are accessed by
specifying the required axis, followed by a double colon, followed by an attribute
name. For example, to access the homepage property of a `WebSite` object on the
*child* axis, you would specify `WebSite[consumer::homepage]`.

The binary operations supported in filter predicates are:

- Equals, written as `eq`
- Greater-Than, written as `gt`
- Less-Than, written as `lt`
- Greater-Then *or* Equals, written as `gteq`
- Less-Than *or* Equals, written as `lteq`
- Contains, written as `contains`
- Starts-With, written as `starts_with`
- Ends-With, written as `ends_with`
- Like (which accepts the wildcard `%`), written as `like`
- Matches (taking a PCRE formatted regular expression), written as `matches`
- Path-Exists (taking an XPath expression), written as `path_exists`

##### Literal Handling

Literal strings should be enclosed in single quotation marks, like so:

    parent::$(display) contains 'Managed'

No syntactic distinction is made between floating point and decimal numbers, as these
are non-mandatory and the same applies to dates versus timestamps. For numbers, no
special treatment is necessary:

    ::max-connections gt 24        # integers
    ::max-load-threshold gt 85.9   # floats

Boolean values are expressed using two built-in constants, `TRUE` and `FALSE`.
These MUST be upper cased in order to be recognised as boolean values. Other,
user defined constants, SHOULD be referenced using the `:<name>` syntax.

Attributes containing `Date` information are also handled specially, by
enclosing a date string in a pseudo-function `DATE(...)`. For example:

    ::date-of-birth > DATE(12-04-1965)

### Composite Filter Predicates

Each step may define multiple filter predicates, using a logical conjunction
(expressed as `AND`) or disjunction (an `OR`) to join them. In the following
example, we filter for customers with either gold account status or a good
credit rating.

    x-account[consumer::ac-status='gold' AND consumer::cr-rating `gt` CR_GOOD]

An implementation MUST evaluate predicates combined in this way by performing a
full match of ALL the conditions given. The fixity of logical junctions is the
same as that of the *join operators*, thus an implementation MUST evaluate them
with precedence given to the expression(s) on the right (so that the expression
`'a' AND 'b' OR 'c'`, is equivalent to `'a' AND ('b' OR 'c')`.

#### Grouping and SubQueries

TODO: WRITE UP THE SPEC FOR SUBQUERIES AND GROUPING........

Because the input to a step is *always* the output of the previous step - with
the exception of the initial (root) step of the query - the default behaviour
of a query is produce its output *depth first*. If we use an indentation based
format - YAML in this case - to represent a simple result set, we can get an
idea of this:

```yaml
# (step1@(a-b => (b-c, b-d)) => step2@(b-k, b-n) => n-z)
a:
    b:
        c
        d
```

Clearly the only outputs from `step1` which are also acceptable inputs for
`step2`, are *b's* and not *c's* or *d's*. Unfortunately only the outputs of
the last step are going to be joined, therefore the `intersection` between
`a-b` has already completed and the union of `b-c, b-d` for each *b* in `a-b`
becomes the output for `step1`, causing `step2` to be empty (as no *b's* have
made it through to become its inputs).

Subqueries solve this grouping problem neatly. The whole subquery is applied
to its target step *depth first*, but the results are effectively dropped when
joining to the following step, such that the outputs of the step to which the
subquery has been applied produces the inputs to the step following the
subquery itself. If we change our example to use a subquery, we will see this
in action:

```yaml
# (step1@(a-b <- (b-c, b-d)) => step2@(b-k, b-n) => n-z)
a:
    b:
        c
        d
        k
        n:
            z
```
----

The order in which vertices are connected is generally the same order in which
they're defined in the query. Thus if we grab order history for Customers,
along with the Products purchased and any special Offers, we will obtain an
order that reflects this:

    # Query
    customerOrders => orderProducts => productOffers

    # Result Set
    ( Customer[Joe Blogs] => Order[1234] )
    ( Order[1234]         => Product[Ice Cream] )
    ( Product[Ice Cream]  => Offer[2-4-1] )

Edges can be grouped together at the same *depth* by applying a `union`, which
by definition returns all its edges at the same distance to the preceding step.
In the following query then, Products and Services are treated as siblings:

    customerOrders => (orderProducts, orderServices) => serviceTerms

The most important point about this grouping is that the edges participating in
the `union`, MUST *all* be connected to the output of the step, such that the
consuming vertices from each of the prior edges are
providing vertices in all of the edges (in all of the resulting edge sets)
within the `union` group.

This example (above) further illustrates the way edges from a `union` are
conjoined in following steps. The `serviceTerms` edges will appear for each
`Service` in the `orderServices` set, but clearly there is no conjoining
association between `Product` and `Service` so we will see a result set that
looks something like this:

    ( 1, Customer['Joe Blogs']      => Order[1234] )
    ( 2, Order[1234]                => Service['Free Delivery'] )
    ( 3, Service['Free Delivery']   => Term[108641] )
    ( 2, Order[1234]                => Product['Ice Cream'] )

This time we'll use XML to get a feel for the traversal order:

```xml
<customer name="Joe Blogs">
    <order id="1234">
        <product name="Ice Cream" />
        <service name="Free Delivery">
            <term id=108641 condition="once-per-order"
                            description="..."
                            level="48hr" />
        </service>
    </order>
</customer>
```

If the model defined a relationship with `Term`, that existed for both
`Product` *and* `Service`, we would see both sets of edges of conjoining nodes
returned by the query (providing of course that the data points exist):

    # Query
    customerOrders => 
        (orderProducts, orderServices) => 
            Term[consumer::policy-mode='Active']

    # Result Set
    ( 1, Customer['Joe Blogs']      => Order[1234] )
    ( 2, Order[1234]                => Product['Ice Cream'] )
    ( 3, Product['Ice Cream']       => Term[1135647] )
    ( 2, Order[1234]                => Service['Free Delivery'] )
    ( 3, Service['Free Delivery']   => Term[108641] )

And again in XML format:

```xml
<customer name="Joe Blogs">
    <order id="1234">
        <product name="Ice Cream">
            <term id="1135647" condition="statutory"
                               description="Manufacturers Guarantee" />
        </product>
        <service name="Free Delivery">
            <term id="108641" condition="once-per-order"
                              description="..."
                              level="best endeavours" />
        </service>
    </order>
</customer>
```

In the preceding example, we moved from an *implicit name predicate* to a more
expressive *Type Name Predicate*, indicating that we're interested in returning
all objects of type `Term` which are associated with consumer asset nodes of
the preceding steps (i.e., any `Term` that is associated with any `Product` in
the `orderProducts` set *or* any `Service` in the `orderServices` set). We also
used a filter predicate on the *Type Name*, to limit our results to `Terms`
which are currently active.

#### Recursive Join Conditions

Sometimes it is necessary to find a recursive path through the input graph. We
illustrate this with the following example:

    +--Role--+
    |        |
    |        | personRoles (^) / roleRelationship (v)
    +        |
    Person---+

Let's take a sample data set for a small management chain:

    Person =>   Role      =>  Person/Relationship
    John   =>   Manager   =>  Julie
    Julie  =>   Manager   =>  Susan

If we started our traversal with `John`, we would need to continue cycling
through the associations between `Person` and `Person` via `Role` in order to
generate a complete graph. If we were to write this query without informing
the OGQL engine about the requirement for cycles, it would terminate too early:

    # Query
    personRoles => roleRelationship

    # Result Set
    ( 1, Person['John']  => Role['Manager'] )
    ( 2, Role['Manager'] => Person['Julie'] )

When we want to follow the recursion in the input graph, we must explicitly
tell the query engine this by prefixing the step with an asterisk. In a self
referential association, we can do this on the *implicit name predicate*
directly. So if the relationship was defined as Person-2-Person, we could apply
the *recursive join operator* directly, like so:

    # Query
    *person-2-person

We need to allow the recursion to traverse the additional association via
`Role` therefore we apply the operator to the `intersection` of both steps
instead:

    # Query
    *(personRoles => roleRelationship)

    # Result Set
    ( 1, Person['John']  => Role['Manager'] )
    ( 2, Role['Manager'] => Person['Julie'] )
    ( 3, Person['Julie]  => Role['Manager] )
    ( 4, Role['Manager'] => Person['Susan'] )

Note that the distance from the start point increments contiguously, which is
a feature of the *traversal-order grouping* that the `intersection` operator
provides.

### Step Aliases and Back-reference join conditions

Let's consider the following query defintion:

    group1@(customer-order => order-supplier)

The `<ref-name>@<step>` syntax is used to provide an *alias* for a step, so that
the *named step* may be used later on, in filter conditions.

Most edges are selected based on a single join condition, being later filtered
by either *implicit name predicate*, *Type Predicate*, *filter predicate* or
some combination of all the above. This *primary join condition* (as it is
known) ensures that each child of a *step* in the query, MUST be the ancestor
of one or more children in the following step, otherwise the path for that node
WILL be terminated.

Whilst it is not possible to remove this *primary join condition*, it is
possible to place additional constraints on the selection of edges, such that
you can enforce the existence of multiple relationships to be present.

We take a simple example: producing an order history. Our history is produced
by selecting an initial `Customer` node, grabbing all the customers `Orders`,
all the `Products` for each `Order` and all the `Offers` for the `Products`.
The query looks like this:

    # Query
    customerOrders => orderProducts => productOffers

    # Result Set
    ( Customer[Joe Blogs] => Order[1234] )
    ( Order[1234]         => Product['Ice Cream'] )
    ( Product[Ice Cream]  => Offer[2-4-1] )

Whilst the results look right, it could be that this `Offer` was not applicable
when the `Order` was placed/purchased, because of date/time limits for example.
In this case, we *could* model the relationship between `Offer` and `Order` in
an association type `orderOffersApplied`. Given this relationship, the OGQL
query can filter the `Offers` by both the `Product` and `Order` like so:

    # Query
    customerOrders => 
        varname@(Product[consumer::type = 'Food']) =>
            productOffers[@varname.provider::^orderOffersApplied]]

    # Result Set
    ( Customer[Joe Blogs] => Order[1234] )
    ( Order[1234]         => Product[Ice Cream] )
    ## note that the incorrectly returned Offer his disappeared now...

The `orderOffersApplied` relationship exists on the `Order` object, which is
the consuming object in the first step of the query. We therefore use a named
variable as our back-reference, pointing to the second step: `@varname`.
By itself, this isn't quite enough, as the in-step predicate is a relationship
name, therefore we need to know whether it is the providing or consuming node
that we should check. The syntax for this is `@<StepRef>.<Axis>`, as in
`@varname.consumer`.

The next bit of syntax we must pay attention to is the accessor in the
predicate. When square brackets appear after an *implicit name predicate*,
they may indicate a back-reference join condition, or a standard
*filter predicate* on one of the two nodes in the edge (provider or consumer).
The presence of a back-reference to `@varname` is enough to know which kind of
condition we're evaluating, but we need to know *how* the target of the
back-reference is related to the consumer nodes in the current set of edges.

Normally the `::` accessor denotes a lookup on either standard asset fields or
a specific attribute type. In this case however, it is prefixed with a `^`
which denotes an association type (provided as an instance of an
*implicit name predicate*). So the condition 
`productOffers[@varname.consumer::^orderOffersApplied]` can be read like this:

    All edges in the productOffers set,
        where a consumer asset of the step aliased with 'varname'
            is connected to the consumer asset of the current (productOffers) set
        as a provider asset in the relationship "orderOffersApplied"

