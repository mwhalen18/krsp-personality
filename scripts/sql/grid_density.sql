select *
	,spr_number / area as spr_density
from
(
	select Grid
		, `Year`
		, count(distinct squirrel_id) as spr_number
		, CASE
			WHEN grid in ('KL', 'SU', 'CH', 'JO') THEN 39.69
			WHEN grid = 'LL' THEN 26.73
			ELSE 45.36
		  END as area
	from
	(
		select reflo
			, squirrel_id
			, grid
			, `date`
			, EXTRACT(Year from `date`) as `Year`
			, Sex
			, CAST(locY as decimal(4,1)) as locY
			,CAST( 
			CASE
				WHEN locX like '%A%' THEN replace(locX, 'A', 1)
				WHEN locX like '%B%' THEN replace(locX, 'B', 2)
				WHEN locX like '%C%' THEN replace(locX, 'C', 3)
				WHEN locX like '%D%' THEN replace(locX, 'D', 4)
				WHEN locX like '%E%' THEN replace(locX, 'E', 5)
				WHEN locX like '%F%' THEN replace(locX, 'F', 6)
				WHEN locX like '%G%' THEN replace(locX, 'G', 7)
				WHEN locX like '%H%' THEN replace(locX, 'H', 8)
				WHEN locX like '%1%' THEN replace(locX, 'I', 9)
				WHEN locX like '%J%' THEN replace(locX, 'J', 10)
				WHEN locX like '%K%' THEN replace(locX, 'K', 11)
				WHEN locX like '%L%' THEN replace(locX, 'L', 12)
				WHEN locX like '%M%' THEN replace(locX, 'M', 13)
				WHEN locX like '%N%' THEN replace(locX, 'N', 14)
				WHEN locX like '%O%' THEN replace(locX, 'O', 15)
				WHEN locX like '%P%' THEN replace(locX, 'P', 16)
				WHEN locX like '%Q%' THEN replace(locX, 'Q', 17)
				WHEN locX like '%R%' THEN replace(locX, 'R', 18)
				WHEN locX like '%S%' THEN replace(locX, 'S', 19)
				WHEN locX like '%T%' THEN replace(locX, 'T', 20)
				WHEN locX like '%U%' THEN replace(locX, 'U', 21)
				WHEN locX like '%V%' THEN replace(locX, 'V', 22)
				WHEN locX like '%W%' THEN replace(locX, 'W', 23)
				WHEN locX like '%X%' THEN replace(locX, 'X', 24)
				WHEN locX like '%Y%' THEN replace(locX, 'Y', 25)
				WHEN locX like '%Z%' THEN replace(locX, 'Z', 26)
			ELSE locX
			END as decimal(4,1)) as locX
		from 
		(
			select reflo COLLATE utf8_general_ci as reflo
					, squirrel_id
					, locX COLLATE utf8_general_ci as locX
					, locY COLLATE utf8_general_ci as locY
					, grid COLLATE utf8_general_ci as grid
					, `date` COLLATE utf8_general_ci as `date`
					, Sex COLLATE utf8_general_ci as Sex
			from dbamidden
			where squirrel_id is not NULL
		UNION
			select	reflo as reflo
				, squirrel_id
				, locx as locX
				, locy as locY
				, gr as grid
				, census_date as `date`
				, sex as Sex
			from census
			where squirrel_id is not NULL
		) cens
	) locs
	where ((grid in ("KL", "SU", "CH", "JO"))
			and (MONTH(`date`) = 5)
			and (locX >= -0.2) 
			and (locX <= 20.8) 
			and (locY >= -0.2) 
			and (locY <= 20.8)) 
		or
		  (grid = "LL"
			and (`Year` > 2005)
			and (MONTH(`date`) = 5)
			and (locX >= -10.2)
			and (locX <= 22.8)
			and (locY >= -0.2) 
			and (locY <= 8.8))
		or 
		  (grid = "AG"
			and (MONTH(`date`) = 5)
			and (locX >= -0.2)
			and (locX <= 20.8)
			and (locY >= -0.2)
			and (locY <= 23.8))
	group by Grid, `Year`
) main

