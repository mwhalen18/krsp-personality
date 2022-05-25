select juv.litter_id
  , lit.grid
  , avg(lit.part) as part
  , lit.year
  , avg(juv.n1_weight) as n1_weight
  , avg(lit.n1_date) as n1_date
  -- , avg(juv.n2_weight) as n2_weight
  -- , avg(lit.n2_date) as n2_date
  , count(distinct juv.squirrel_id) as litter_size
from 
(
  select squirrel_id
    , litter_id
    , weight as n1_weight
    , tagWT as n2_weight
  from juvenile
  where (weight is not null)
    and (weight > 0)
) juv
left join
(
  select id as litter_id
    , grid
    , DAYOFYEAR(date1) as n1_date
    , DAYOFYEAR(tagDt) as n2_date
    , DAYOFYEAR(fieldBDate) as part
    , squirrel_id as dam_id
    , extract(year from date1) as year
  from litter
  where date1 is not null
) lit
on juv.litter_id = lit.litter_id
where lit.n1_date is not null
group by lit.litter_id, lit.year