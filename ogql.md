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
provided as recommendations elsewhere in the [[EAV specifications]].

#### Traversal Rules

The rules governing traversal are fairly simple. Each edge must be directly
connected to its immediate ancestor via the providing end of the association
(i.e., the left node). For every edge selected, both nodes MUST satisfy ALL the
conditions declared in the *step* for the edge to be included in the result set.

Cycles are not permitted - that is, the API allows you to produce an acyclic graph
only. A graph containing cycles may be used as input to the query, in which case
the implementation MUST prevent infinite recursion from taking place.

#### Basic Syntax

The basic syntax of an OGQL query is expressed by the following PEG grammar:

```
query                       <- head:step tail:(sep step)* ;
step                        <- normative_step / recursive_step ;
normative_step              <- root_branch_filter / name_predicate / 
                                type_predicate / filter_predicate / grouping;
recursive_step              <- '*' normative_step ;
root_branch_filter          <- '$root(' (!')' root_identifier)+ ')' ;
root_identifier             <- primary_asset_id ('-' semver)? ;
primary_asset_id            <- index+ / asset_name+ ;
grouping                    <- fixed_order_group / traversal_order_group ;
fixed_order_group           <- ('(' (!')' query)* ')') ;
traversal_order_group       <- ('{' (!'}' query)* '}') ;
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
operator                    <- '=' / '>' / '<' / '!=' / '@' /
                                'eq' / 'gt' / 'lt' / 'neq' / 'lteq' / 'gteq' /
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
asset_name                  <- [\\w_]+ ;
semver                      <- ([0-9]+ '.' [0-9]+ '.' [0-9]+) ;
word                        <- [\\w\\-_]+ ;
space                       <- [ \\t\\n\\s\\r] / crlf ;
crlf                        <- [\\r]? [\\n] ;
sep                         <- (space+)? ',' (space+)? ;

```

### Defining the Behaviour of the Query Engine

Because the physical storage model used by The Repository is not part of the set
of the EAV specifications, the execution of OGQL queries is implementation
dependant. The outward behaviour of the query engine however, can be defined as
follows.

#### Joining

An OGQL query is broken up into a set of ordered steps, separated by commas. Each
step describes a set of edges in the underlying object graph which meets certain
criteria. For example, the query `cond1,cond2` means, all edges matching `cond1`
and all adjoining edges matching `cond2`.

With the exception of the right hand side nodes (the providers) on the root
branch, the parent/provider vertex (node) of each edge MUST be a child/consumer
vertex (node) from the output of the previous step. To make this clear, we can
look at the following graph, represented as a list of edges (x, y):

    [(b, d), (c, e), (a, b), (b, c), (e, f), (n, k), (k, v)]

The adjacency list for this graph, starting at `a` would look like this:

    distance      path
    ---------------------
    1             a => b
    2             b => c
    3             c => e
    4             e => f
    2             b => d

Note that the edges `(n, k), (k, v)` are *not* included in the results, because
they are not found anywhere on the path from the starting point `a`. Understanding
this constraint is vital. The actual conditions/criteria used to filter the edges
will now be explained.

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
specific `Supplier`, which would be achieved with the query `whstock,stockSupplier`.
In this case, the edges identified in the first step would be used to filter over
all the *stockSupplier* edges based on the `Stock` nodes themselves, guaranteeing
that you only link to suppliers whose `Stock` you currently hold.

#### Binary Operators in Predicates

Predicates MUST support all the binary operators defined by the
[[Search API |SEARCH]]. Binary operators that have an OPTIONAL symbol associated
with them may be referenced using this alias, so comparisons such as
`parent::$(version.major) > 1` may be made. Other operators which have a name rather
than a symbol, must be separated from their operands by a single space, for example

    parent::$(name) contains 'TC'

#### Literal Handling

The literals used in predicates/expressions can be expressed using any mandatory 
primitive type defined in the [[core datatypes specification|CORE_DATATYPES|]], with 
the exception of the `Document` data type, which isn't likely to be used in join 
conditions. Literal strings should be enclosed in single quotation marks, like so:

    parent::$(display) contains 'Managed'

No syntactic distinction is made between floating point and decimal numbers, as these
are non-mandatory and the same applies to dates versus timestamps. For numbers, no
special treatment is necessary:

    ::max-connections > 24        # integers
    ::max-load-threshold > 85.9   # floats

Boolean values are expressed using two built-in constants, `TRUE` and `FALSE`. 
These MUST be upper cased in order to be recognised as boolean values. Other,
user defined constants, SHOULD be referenced using the `:<name>` syntax.

Fields containing `Date` information are also handled specially, by enclosing a date
string in a pseudo-function `DATE(...)`. For example:

    ::date-of-birth > DATE(12-04-1965) 

The same (pseudo-function) approach can be used to access an assets version fields as
a single entity, so that the following predicates are equivalent:

    # long winded version
    ::$(version.major) = 0 AND
    ::$(version.minor) = 13 AND
    ::$(version.build) = 63

    # simpler version
    ::$(version) = VSN(0.13.63)

#### Accessing edge nodes in Predicates

Access to the assets nodes (connected by an association) is achieved by specifying
the axes on which the node lies. The two axes which MUST be supported are *parent*
and *child* which refer to the left and right nodes of the association
respectively. Implementations MAY alias these axes using the terms *provider* (for
parent) and *consumer* (for child). Attributes of the edge nodes is performed by
specifying the required axis, followed by a double colon, followed by the field
name. For example, to access the homepage property of a `WebSite` object on the
*child* axis, you would specify `consumer::homepage`.

Access to core asset attributes (i.e., those which are not user defined) is
achieved using a special variable name, which starts with a dollar sign ($) and is
surrounded by parenthesis. The Repository MUST take these special variable names 
and look up the core attribute with the same name (or alias). 
The supported attributes are:

    +----------------------+------------------------------------------------------------------------------------+
    | Variable             | Field Access                                                                       |
    +----------------------+------------------------------------------------------------------------------------+
    | $(name)              | the asset name                                                                     |
    | $(description)       | the asset display name                                                             |
    | $(display)           | alias for `$(description)`                                                         |
    | $(key)               | the *business key* for the asset (see [[MODEL]] for more details on business keys) |
    | $(version)           | the *version string* (i.e., the 3-4 part version number represented as a string)   |
    | $(version.major)     | the *major version number*                                                         |
    | $(version.minor)     | the *minor version number*                                                         |
    | $(version.patch)     | the *patch version number*                                                         |
    | $(lastmodified.date) | the *last modified date*                                                           |
    | $(lastmodified.user) | the *last modified user*                                                           |
    | $(created.date)      | the *created date*                                                                 |
    | $(created.user)      | the *created by user*                                                              |
    +----------------------+------------------------------------------------------------------------------------+

### Finding the Entry Point - Root Branch Filtering

*Root Branch Filtering* is a special case, because often you want to build a graph
from a specific starting point. Consumers of the [[RESTAPI]] can achieve this by
simply adding a path component to the URL with the required business key (or asset
name) and can provide a second additional path to specify the exact version
required.

Consumers not going via the [[RESTAPI]] may either set up a filter against the
parent axes, or supply a *special value* as the first path step in the query. This
*special value* is wrapped in a pseudo-function `$root()` and consists of the 
business key (or asset name) and OPTIONALLY a trailing hyphen followed by the 
required version number. Whilst this approach is less powerful than a full filter
predicate, it is easier to write for simple cases (which are usually the norm).
Here is an example demonstrating the difference between the two approaches.

    # defining a filter on the parent
    platform-service[provider::$(key) = 'B2B' AND provider::$(version) = VSN(1.0.0)],
        service-interface,interface-operation
    
    # defining the root branch filter using the alternative syntax
    $root(B2B-1.0.0),platform-service,service-interface,interface-operation

### Composite Filter Predicates

Each step may define multiple filter predicates, using a logical conjunction 
(expressed as `AND`) or disjunction (an `OR`) to join them. In the following 
example, we filter for customers with either gold account status or a good credit rating.

    x-account[consumer::ac-status='gold' AND consumer::cr-rating `gt` CR_GOOD]

The Repository MUST evaluate predicates combined in this way by performing an
inclusive match. These logical junctions MUST be evaluated with precedence given
to the expression(s) on the right, so that the expression `'a' AND 'b' OR 'c'`,
is equivalent to `'a' AND ('b' OR 'c')`.

### Back-reference join conditions

Most edges are selected based on a single join condition, being later filtered by
either *implicit name predicate*, *Type Predicate*, *filter predicate* or some
combination of all the above. This *primary join condition* (as it is known) 
ensures that each child of a *step* in the query, MUST be the ancestor of one or 
more children in the following step, otherwise the path for that node WILL be
terminated.

Whilst it is not possible to remove this *primary join condition*, it is possible
to place additional constraints on the selection of edges, such that you can
enforce the existence of multiple relationships to be present.

We take a simple example: producing an order history. Our history is produced by
selecting an initial `Customer` node, grabbing all the customers `Orders`, all the
`Products` for each `Order` and all the `Offers` for the `Products`. The query
looks like this:

    # Query
    $root('Joe Blogs'),customerOrders,orderProducts,productOffers

    # Result Set
    ( Customer[Joe Blogs] => Order[1234] )
    ( Order[1234]         => Product['Ice Cream'] )
    ( Product[Ice Cream]  => Offer[2-4-1] )

Whilst the results look right, it could be that this `Offer` was not applicable
when the `Order` was placed/purchased, because of date/time limits for example.
In this case, we *could* model the relationship between `Offer` and `Order` in an
association type `orderOffersApplied`. Given this relationship, the OGQL query can
filter the `Offers` by both the `Product` and `Order` like so:

    # Query
    $root('Joe Blogs'),
        customerOrders,
        orderProducts,
            productOffers[@1.consumer::^orderOffersApplied]

    # Result Set
    ( Customer[Joe Blogs] => Order[1234] )
    ( Order[1234]         => Product[Ice Cream] )
    ## note that the incorrectly returned Offer his disappeared now...

The `orderOffersApplied` relationship exists on the `Order` object, which is the
consuming object in the first step of the query. We therefore use a numeric index
as our back-reference, pointing to the first step: `@1`. By itself, this isn't
quite enough, as the in step predicate is a relationship name, therefore we need
to know whether it is the providing or consuming node that we should check. The
syntax for this is `@<Index>.<Node>`, as in `@1.consumer`.

The next bit of syntax we must pay attention to is the accessor in the predicate.
When square brackets appear after an *implicit name predicate*, they may indicate
a back-reference join condition, or a standard *filter predicate* on one of the
two nodes in the edge (provider or consumer). The presence of a back-reference to
`@1` is enough to know which kind of condition we're evaluating, but we need to
know *how* the target of the back-reference is related to the consumer nodes in
the current set of edges. Normally the `::` accessor denotes a lookup on either
standard asset fields or a specific attribute type. In this case however, it is
prefixed with a `^` which denotes an association type (provided as an instance of
an *implicit name predicate*). So the condition
`productOffers[@1.consumer::^orderOffersApplied]` can be read like this:

    All edges in the productOffers set,
        where a consumer asset of step 1
            is connected to the consumer asset of the current (productOffers) set
        as a provider asset in the relationship "orderOffersApplied"

#### Grouping and Ordering

There are two kinds of grouping operators available in OGQL. The first is known as
an *fixed-order group* and represents a set of nodes that take their input
from the consuming assets of the step declared immediately before the group.
The second, known as a *traversal-order group* represents a set of nodes that
flow from one another as though no grouping was being applied (i.e., for each
step in the group, the input nodes come from the previous step and the distance
continues to grow), but whose outputs are treated as a single set when joining
the edges in the next step (following the end of the group).

The order in which vertices are connected is generally the same order in which 
they're defined in the query. Thus if we grab order history for our customers, 
along with the products purchased and any special offers, we will obtain an 
order that reflects this:

    # Query
    $root('Joe Blogs'),customerOrders,orderProducts,productOffers

    # Result Set
    ( Customer[Joe Blogs] => Order[1234] )
    ( Order[1234]         => Product[Ice Cream] )
    ( Product[Ice Cream]  => Offer[2-4-1] )

Edges can be grouped together as part of the query. Grouping can be used to
return edges at the same *depth*, as in the following query where products and
services are treated as siblings:

    customerOrders,(orderProducts,orderServices),serviceTerms

The most important point about this grouping is that the edges must *all* be
connected to the output of the step prior to the application of the grouping
operators, such that the consuming assets from each of the prior edges are
providing assets in all of the edges (in all of the resulting edge sets)
within the group. This is a *fixed order group*.

This example (above) also illustrates an important point about how edges from a
grouping are conjoined in following steps. The `serviceTerms` edges will
appear for each `Service` in the `orderServices` set of edges, but clearly there
is no conjoining association between `Product` and `Service` so we will see a
result set that looks something like this:

    ( 1, Customer['Joe Blogs']      => Order[1234] )
    ( 2, Order[1234]                => Service['Free Delivery'] )
    ( 3, Service['Free Delivery']   => Term[108641] )
    ( 2, Order[1234]                => Product['Ice Cream'] )

When represented using a hierarchical representation such as XML, we might find
this result set look a bit like this:

    <customer name="Joe Blogs">
        <order id="1234">
            <product name="Ice Cream" />
            <service name="Free Delivery">
                <term id=108641 condition="once-per-order"
                                description="..."
                                level="best endeavours" />
            </service>
        </order>
    </customer>

If the model defined a relationship with terms, that existing for both products
and services, we would see both sets of edges of conjoining nodes returned by the
query, providing of course that the data points existed:

    # Query
    customerOrders,(orderProducts,orderServices),Term[consumer::policy-mode='Active']

    # Result Set
    ( 1, Customer['Joe Blogs']      => Order[1234] )
    ( 2, Order[1234]                => Product['Ice Cream'] )
    ( 3, Product['Ice Cream']       => Term[1135647] )
    ( 2, Order[1234]                => Service['Free Delivery'] )
    ( 3, Service['Free Delivery']   => Term[108641] )

    # Sample Representation
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

This conjoining of the outputs of the group to the following step applies to
both kinds of grouping.

In the preceding example, we moved from an *implicit name predicate* to a more
expressive *Type Name Predicate*, indicating that we're interested in returning
all objects of type `Term` which are associated with consumer asset nodes of
the preceding steps (i.e., any `Term` that is associated with any `Product` in
the `orderProducts` set *or* any `Service` in the `orderServices` set). We also
used a filter predicate on the *Type Name Predicate* itself, to limit our results
to `Terms` which are currently active.

The application of *traversal-order groups* is quite different. The inputs to
each step come from the immediately preceding step, regardless of whether it
sits within the group or not. For example

    # Query
    $root(factory),{factory-process,process-subprocess},process-owner

Even without a sample data set, it should be apparent that the operation of this
query is quite different to one which uses the *fixed-order grouping* operators.
If we had stated the query as `(factory-process,process-subprocess)` there would
have been *no* inputs into the `process-subprocess` step, as no step precedes the
group to provide them.

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

If we started our traversal with `John`, we would need to continue cycling through
the associations between `Person` and `Person` via `Role` in order to generate a
complete graph. If we were to write this query without informing the OGQL engine
about the requirement for cycles, it would terminate too early:

    # Query
    $root(John),personRoles,roleRelationship
    
    # Result Set
    ( 1, Person['John']  => Role['Manager'] )
    ( 2, Role['Manager'] => Person['Julie'] )

When we want to follow the recursion in the input graph, we must explicitly tell
the query engine this by prefixing the step with an asterisk. In a self
referential association, we can do this on the *implicit name predicate* directly.
So if the relationship was defined as Person-2-Person, we could apply the
*recursive join operator* directly, like so:

    # Query
    $root(John),*person-2-person

We need to allow the recursion to traverse the additional association via `Role`
therefore we apply the operator to the grouping of both steps instead:

    # Query
    $root(John),*{personRoles,roleRelationship}
    
    # Result Set
    ( 1, Person['John']  => Role['Manager'] )
    ( 2, Role['Manager'] => Person['Julie'] )
    ( 3, Person['Julie]  => Role['Manager] )
    ( 4, Role['Manager'] => Person['Susan'] )

Note that the distance from the start point increments continguously,
which is a feature of the *traversal-order grouping* operators to 
which we're applying the *recursive join operator*.

## Limitations of the Current Specification

The current specification is deliberately limited in the following ways.

- It only supports the identification of edges based on association type name
- It only supports filtering edges across two axes
    - the root parent/provider asset
    - the parent/provider or child/consumer asset node on a single edge
- It only supports filtering based on core asset fields or attributes

Future versions of the specification may propose enhancements to this approach, including

- Identifying edges based on the provider/parent or child/consumer asset type
- Identifying edges based on the lifecycle status of the provider/parent and/or child/consumer
- Filtering based on the lifecycle status of the provider/parent and/or child/consumer
- Filtering based on ancestors of the current edge
- Filtering using external (user defined) functions

An example that doesn't work well with the current implementation is the traversal
of warehouse stocks by supplier contract. Let's reconsider the model for a moment:

	              			(stockSupplier)
	    +----------+ Stock +---------------+ Supplier
	    |                                       |
	    | (whstock)                             | (supContract)
	    +             (whcontract)              +
	Warehouse +----------------------------+ Contract

If we want to group our first branch by warehouse and then traverse the stocks but
group these by supplier contract, we're a bit stuffed. It is possible to issue a
query such as `whstock,stockSupplier,supContract` but we get the supplier and
contract nodes repeated for each stock item! That's hardly ideal. What we really
want is to collect all the suppliers who are contracted to provide the particular
warehouse with stock, and under each supplier/contract collect the stock items
which are present in that warehouse. If we try this, we're going to get some fishy
results: `whcontract,!supContract,!stockSupplier` will return the right data until
you hit the last *implicit name predicate* at which point you're pulling all Stock
supplied by that Supplier to all warehouses - there's no constraint on the last
set of edges.

You could tighten up your model to accommodate this use case, perhaps introducing
an additional association type or by using a link asset. That still remains hardly
ideal.


Examples
========

