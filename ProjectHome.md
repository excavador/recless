Recless is a type inferring Erlang parse transform.
Instead of writing these lines:

> City = ((Project#project.owner)#person.address)#address.city,

> NewProject =
> > Project#project{owner =
> > > (Project#project.owner)#person{address =
> > > > ((Project#project.owner)#person.address)#address{city =
> > > > > 'Boston'}}}.

it lets your write these lines:


> City = Project.owner.address.city,

> NewProject = Project.owner.address.city = 'Boston'.

WARNING: RECLESS IS EXPERIMENTAL AND NOT YET FINISHED!!!



