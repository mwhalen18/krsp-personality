select *
	, CASE
		WHEN EXP = "f"
			THEN 19
		ELSE 1
        END as Exp_label
from 
	(select `Year`
		, Grid
		, SUM(NumNew) as num_trees
		, AVG(NumNew) as cone_counts
		, AVG(cone_index) as cone_index
		, CASE
			WHEN `Year` in (1993, 1998, 2005, 2010, 2014, 2019)
				THEN 1
			ELSE 0
			END as mast
		, CASE
			WHEN (Grid = "AG") and (`Year` between 2004 and 2018)
				THEN "f"
			WHEN (Grid = "JO") and (`Year` between 2006 and 2013)
				THEN "f"
			WHEN (Grid = "LL") and (`Year` between 2005 and 2012)
				THEN "f"
			ELSE "c"
			END as EXP
	from
		(select Grid
			, `Year`
			, LocX
			, LocY
			, CAST(NumNew as unsigned) as NumNew
			, ln(NumNew + 1) as cone_index
			, 1.11568 * exp(0.1681 + 1.1891 * ln(NumNew + 0.01)) as total_cones
		from cones
		where (Grid in ("AG", "CH", "JO", "KL", "LL", "SU", "BT")) 
			and (`Year` >=1988)
			-- and (NumNew is not NULL)
	) cone_counts
	group by `Year`, `Grid`
) grid_cones_years