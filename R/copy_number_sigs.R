# categorise CN segments
copyNumberInfo = function(chrom,start,end,CN,CNb=NULL,doVariables=TRUE)
	{
	# segment lengths
	print("lengths")
	lengths = (end-start)/1000000
	if(!is.null(CNb))
		{
		# allelic imbalance
		print("imbalance")
		CNa = CN-CNb
		imba = abs(CNa-CNb)
		# LOH
		LOH=pmin(CNa,CNb)
		} else {
		imba=NULL
		LOH=NULL
		}
	# combine
	print("combine")
	if(!doVariables)
		{
		combined = list(CN=CN,
			lengths=lengths,
			LOH=LOH)
		return(combined)
		}
	LOHstatus = ifelse(LOH==0,"LOH","het")
	LOHstatus[which(CN==0)] = "homdel"
	variables = table(paste(cut(CN,
				breaks=c(-0.5,1.5,2.5,4.5,Inf),
				labels=c("del","neut","dup","amp")),
			LOHstatus,
			cut(lengths,breaks=c(-0.01,0.01,0.1,1,10,Inf)),
			sep=":"))
	return(variables)
	}
