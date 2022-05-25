select juvenile.squirrel_id
  , juvenile.litter_id
  , litter.fieldBDate
  , juvenile.litter_size
  , litter.n1_date
  , juvenile.n1_weight
  , litter.n2_date
  , juvenile.n2_weight
  , litter.dam_id
from 
(
  select a.squirrel_id
    , a.litter_id
    , a.weight as n1_weight
    , a.tagWT as n2_weight
    , litter_size
  from  juvenile as a
  join (
    select litter_id
      , count(squirrel_id) as litter_size
    from juvenile
    group by litter_id
  ) as b
  on a.litter_id = b.litter_id
) as juvenile
left join
(
  select id as litter_id
    , fieldBDate
    , date1 as n1_date
    , tagDt as n2_date
    , squirrel_id as dam_id
  from litter
) as litter
on juvenile.litter_id = litter.litter_id
order by juvenile.litter_id desc
