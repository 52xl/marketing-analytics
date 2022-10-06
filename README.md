# marketing-analytics

## objective of the project:
data coming from the cashes of a retailer in a
Central Italy Province (8 stores in total) in a given period of time.
The company currently does not run much in terms of
CRM of customised marketing action. It has a loyalty program (1 point per € spent) with
the typical rewards (e.g., bathrobes, pieces of luggage, kitchenware, etc.), and it wants
to find out how to approach a more targeted marketing activity


## Dataset

### Transaction DB:
* #### Store:
in which each record is a line in the a ticket. These are the fields in each record:
 Store: identifier of the store. 8 stores are under analysis. Store 576 is theflagship store
(900 sqm) and is located in the suburbs of the main city in the province
(90.000 inhabitants); 519 and 552 are middle sized for the retailer (600
sqm) and are located in two relatively large towns in the province, while the other are small
stores (below 400 sqm) in the main city and in small towns

* #### Ticketdate:
date and time (YYYYMMDDhhmmss) of the ticket

* #### Ticket_id: 
combination of information about the cash line and the number of the ticket
in the day, aimed at distinguishing contemporary tickets (useful especially
when non-cardholders have a ticket in the same place at the same time)

* #### id_cust: 
number of the loyalty card of the customer

* #### Qty: 
number of units of the item purchased

* #### Totprice:
total price paid for the qty units of the item purchased
(i.e., if 2 units each one costing 1.8€ are present,totprice will be equal to 3.6€)

* #### Prod_id: 
code identifying the items

### Product DB:
where the prod_id are described. Each product is identified by a 6-digit code
(if 5 digits are present, it meansthat the first one is 0). The first two digits represent 
families of categories, the second two digits represent product categories
and the last two represent subcategories.

## Output

### RFM analysis:
Rule-based clustering; we want to cluster customers according to three main dimensions:
* #### Recency: How recently has the customer bought?
* #### Frequency: How often does the customer buy?
* #### Monetary: How much does the customer spend in total/on average?

### Market basket analysis:
it is a data mining method focusing on discovering purchasing patterns by extracting
associations or cooccurrences from transactional data. Market basket analysis determines the products
which are bought together 

