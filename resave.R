library('stringr')
args = commandArgs(trailingOnly=TRUE)
resave = function(file){
	e = new.env(parent=emptyenv())
	load(file, envir = e)
	objs = ls(envir = e, all.names = TRUE)
	lower_name = str_to_lower(file)
	base_name = str_replace_all(lower_name, c('.rdata'='', '.Rdata'='', '.RData'='', '.RDATA'=''))
	for(obj in objs) {
		.x = get(obj, envir =e)
		#need to change this output file
		output_name= paste(base_name, obj, 'csv', sep='.')
		path_out= '/summary_files/'
		message(sprintf('Saving %s as %s', obj, output_name) )
		write.csv(.x, file=paste(#need to change this piece too path_out#,output_name,sep=' '))
	}
}

print(paste('checking', args[1], sep=' '))
if (str_detect(args[1], 'rdata') | str_detect(args[1], 'Rdata') | str_detect(args[1], 'RData')) {
	resave(args[1])
}




