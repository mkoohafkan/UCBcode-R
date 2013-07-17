query_with_props <- function(basefields, basetbl, nodefields, nodetbl='node_properties'){ 
	paste(
		'SELECT', 
		paste(basetbl, '.', basefields, sep='', collapse=', '), 
		paste(nodetbl, '.', nodefields, sep='', collapse=', '),
		'FROM', nodetbl, 'LEFT OUTER JOIN', nodetbl, 'ON', 
		paste(basetbl, '.nodeid = ', nodetbl, '.nodeid', sep='')
	)
}

get_nodeprops <- function(fields){
	paste('SELECT', paste(fields, collapse=", "), 'from node_properties')
}

get_summer_ranks <- function(field, tbl, ptile=1, desc=TRUE){
# desc=TRUE means percent ranking is based on 'field' in descending order
# desc=FALSE means percent ranking is based on 'field' in ascending order
if(desc){
	ord <- "DESC"
} else {
	ord <- "ASC"
}
paste('SELECT nodeid, result_time, ', field, ', pc FROM ',
      '(SELECT percent_rank() OVER (PARTITION BY a.nodeid ORDER BY a.', field,
      ' ', ord, ') AS pc, a.nodeid, a.result_time, a.', field, ' FROM ',
	  '(SELECT nodeid, result_time, ', field, ' FROM ', tbl, ' WHERE ',
	  "date_part('month'::text, result_time) > 5::double precision AND ",
	  "date_part('month'::text, result_time) < 9::double precision) a) q ",
	  'WHERE pc <= ', ptile, " ORDER BY pc, nodeid", sep='')
}

sync_query <- function(n1, n2, tbl, field, wherestring=NULL){
# n1 = the first nodeid
# n2 = the second nodid
# basetbl = the table containing records of both n1 and n2
paste('WITH recs AS (',
		  'WITH f AS', 
			  '(SELECT nodeid, result_time,', field, 
			  'FROM', tbl, 
			  'WHERE nodeid = ',n1,' ),',
		  's AS',
			  '(SELECT nodeid, result_time,', field, 
			  'FROM', tbl, 
			  'WHERE nodeid = ',n2,')',
		  'SELECT result_time, nodeid,', field, 
		  'FROM f',
		  'WHERE result_time IN (SELECT result_time from s)',
		  'UNION ALL',
		  'SELECT result_time, nodeid,', field, 
		  'FROM s',
		  'WHERE result_time IN (SELECT result_time from f)',
	  ')',
	  'SELECT nodeid, result_time,', field, ',',
	  'PERCENT_RANK() OVER (PARTITION BY nodeid ORDER BY', field, 
	  ') AS pc FROM recs', wherestring,
	  'ORDER BY nodeid, result_time', sep= ' ')
}

get_diffs <- function(field, tbl){
paste('WITH diffs AS ( WITH recs AS ( SELECT result_time, nodeid, ', field, 
      ', ROW_NUMBER() OVER (PARTITION BY nodeid ORDER BY result_time) AS rownum ',
	  'FROM ', tbl, ' ORDER BY result_time, nodeid) ',
	  'SELECT r1.result_time, r1.nodeid, r1.', field, ' AS current_temp, r1.rownum, r2.', field, 
	  ' AS previous_temp, r2.rownum FROM recs r1 ',
	  'LEFT OUTER JOIN recs r2 ON r1.rownum = r2.rownum + 1 AND r1.nodeid = r2.nodeid) ',
	  'SELECT result_time, nodeid, coalesce(current_temp - previous_temp, NULL) AS temperature_change ', 
	  'FROM diffs ORDER BY nodeid, result_time', sep="")
}