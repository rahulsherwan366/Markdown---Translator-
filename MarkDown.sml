(*--------------------------------------------------------------------------------------------------*)
(*-------------------------------- BEGIN OF I/O FUNCTIONS ------------------------------------------ *)
(*--------------------------------------------------------------------------------------------------*)

fun join (glue, []) = ""
  | join (glue, [s]) = s
  | join (glue, (h::t)) = h^glue^(join (glue, t))

fun lick (filename:string,()) =
    let val f = TextIO.openIn filename
        fun loop (accum: string list) =
            case (TextIO.inputLine f) of 
        NONE => accum
              | SOME line => loop (line::accum)
            (* esac *)
        val lines =   rev(loop [])
    in TextIO.closeIn f; lines
    end


fun write (filename: string, s) = 
    let val f =  TextIO.openOut filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end

fun writeLine (filename: string, s) = 
    let val f =  TextIO.openOut filename
    in  (TextIO.output (f, s^"\n"); TextIO.closeOut f) 
    end

fun appendLine  (filename: string, s) = 
    let val f =  TextIO.openAppend filename
    in  (TextIO.output (f, s^"\n"); TextIO.closeOut f) 
    end

fun appendLines (filename: string, sl) =
    let val ss =  join ("\n", sl)
    in  appendLine (filename: string, ss)
    end

fun writeLines (filename: string, sl) =
    let val ss =  join ("\n", sl)
    in  writeLine (filename: string, ss)
    end

exception underflow 

(*-------------------------------------------------------------------------------------------------*)
(*-------------------------------- END OF I/O FUNCTIONS ------------------------------------------ *)
(*-------------------------------------------------------------------------------------------------*)


(*-------------------------------------------------------------------------------------------------*)
(*------------------SOME COMMON FUNCTIONS USED BY OTHER FUNCTIONS -------------------------------- *)
(*-------------------------------------------------------------------------------------------------*)

fun find(a,b)= String.isSubstring a b

fun convertstring [] = ""
    | convertstring (h::t) = h^convertstring(t)

fun evaluate a = (*if (String.extract(a,(String.size(a)-1),NONE)) ="\n"*)
                 (*then*) String.extract(a,0,SOME (String.size(a)-1))
                 (*else a*)
                    
(*-------------------------------------------------------------------------------------------------*)
(*------------------END OF COMMON FUNCTIONS ------------------------------------------------------ *)
(*-------------------------------------------------------------------------------------------------*)


(*-----------------------------------------------------------------------------------------*)
(*----------------------------FUNCTIONS TO DEAL WITH "\n"--------------------------------- *)
(*-----------------------------------------------------------------------------------------*)

fun convert0 [] = []
    |convert0 (h::t) = if h = "\n"
                       then convert0(t)
                       else h::t@["</p>"]

fun convert5 []=[]
    | convert5 (h::[]) = [h]
    | convert5 (h::t) = if h <> "\n" 
                        then evaluate(h)::convert5(t)
                        else if h = "\n" andalso hd(t) = "\n"
                        then convert5(t)
                        else h::convert5(t)

fun addendline [] = []
    | addendline a = if hd(a)=""
                   then hd(a)::"\n"::addendline(tl(a))
                   else hd(a)::addendline(tl(a))

(*-----------------------------------------------------------------------------------------*)
(*---------------------------END OF "\n" FUNCTIONS-----------------------------------------*)
(*-----------------------------------------------------------------------------------------*)


(*-----------------------------------------------------------------------------------------*)
(*FUNCTIONS TO DEAL WITH "#" AND CONVERT IT TO HTML'S RESPECTIVE TAG i.e. <H1>HEADING </H2>*)
(*-----------------------------------------------------------------------------------------*)
fun count [] = 0
    | count (h::t) = if (Char.compare(h,#"#")=EQUAL)
                     then let fun c [] = 0
                     			 |  c (h::t) = if (Char.compare(h,#"#")=EQUAL)
                                            then 1+c(t)
                                            else 0
                        in c(h::t)
                        end
                     else 0+count(t)

fun remove [] = []
    | remove (h::t) = if (Char.compare(h,#"#")=EQUAL)
                     then let fun c [] = []
                     			| c (h::t) = if (Char.compare(h,#"#")=EQUAL)
                                            then c(t)
                                            else h::t
                        in c(h::t)
                        end
                     else h::remove(t)

fun counthash a = Int.toString(count(String.explode(a)))

fun removehash a = String.implode(remove(String.explode(a)))

fun convert3 []=[]
    | convert3 (h::t) = if find("#",h) = true
                        then ("<h"^(counthash(h))^">"^evaluate(removehash(h))^"</h"^(counthash(h))^">\n")::convert3(t)
                        else h::convert3(t)

(*-----------------------------------------------------------------------------------------*)
(*---------------------------END OF "#" FUNCTIONS------------------------------------------*)
(*-----------------------------------------------------------------------------------------*)

(*--------------------------------------------------------------------------------------------*)
(*FUNCTIONS TO DEAL WITH "PARAGRAPHS" AND CONVERT IT TO HTML'S RESPECTIVE TAG i.e. <P>TEXT</P>*)
(*--------------------------------------------------------------------------------------------*)
 
 fun calc [] = []
    | calc (h::[]) = [h]
    | calc (h::t::[]) = [h,t]
    | calc (h1::h2::t) = if h2 = #"\n" 
                        then h1::(#"<")::(#"/")::(#"p")::(#">")::(#" ")::h2::(#" ")::(#"<")::(#"p")::(#">")::calc(t)
                        else h1::calc(h2::t)
 fun convert4 a = implode(calc(String.explode(a)))

(*-----------------------------------------------------------------------------------------*)
(*---------------------------END OF "PARAGRAPH" FUNCTIONS----------------------------------*)
(*-----------------------------------------------------------------------------------------*)


(*---------------------------------------------------------------------------------------------------------------*)
(*FUNCTIONS TO DEAL WITH "ORDERED LIST" AND CONVERT IT TO HTML'S RESPECTIVE TAG i.e. <OL>...<LI>TEXT</LI>...</OL>*)
(*---------------------------------------------------------------------------------------------------------------*)
 
fun findnumpos1 [] = raise underflow 
    | findnumpos1 (h::t) = if (Char.compare(h,#"."))= EQUAL   
                    then 0
                    else 1 + findnumpos1(t)
fun removenumber a = String.extract(a,findnumpos1(String.explode(a))+1,NONE)
fun taglist [] =[]
    | taglist (h::t) = if find("0. ",h) = true orelse find("1. ",h) = true orelse find("2. ",h) = true orelse find("3. ",h) = true orelse find("4. ",h) = true orelse find("5. ",h) = true orelse find("6. ",h) = true orelse find("7. ",h) = true orelse find("8. ",h) = true orelse find("9. ",h) = true  
                       then ("<li><p>"^removenumber(h)^"</li>")::taglist(t)
                       else []

fun removelist [] = [] 
    | removelist (h::t) = if find("0. ",h) = true orelse find("1. ",h) = true orelse find("2. ",h) = true orelse find("3. ",h) = true orelse find("4. ",h) = true orelse find("5. ",h) = true orelse find("6. ",h) = true orelse find("7. ",h) = true orelse find("8. ",h) = true orelse find("9. ",h) = true  
                          then removelist(t)
                          else h::t
fun orderedlist [] =[]
    | orderedlist (h::t) = if find("0. ",h) = true orelse find("1. ",h) = true orelse find("2. ",h) = true orelse find("3. ",h) = true orelse find("4. ",h) = true orelse find("5. ",h) = true orelse find("6. ",h) = true orelse find("7. ",h) = true orelse find("8. ",h) = true orelse find("9. ",h) = true  
                           then ["<ol>"]@taglist(h::t)@["</ol>"]@orderedlist(removelist(t))
                           else h::orderedlist(t)


fun findnum1pos [] = raise underflow 
    | findnum1pos (h::t) = if (Char.compare(h,#"."))= EQUAL   
                    then 0
                    else 1 + findnum1pos(t)
fun  taglist1 a =  String.substring(a,0,findnum1pos(String.explode(a)))
fun  taglist2 a = String.extract(a,findnum1pos(String.explode(a))+1,NONE)
fun  tagmlist [] = []
    | tagmlist (h::t) = if h = "\n"
                         then []
                         else if find("0. ",h) = true orelse find("1. ",h) = true orelse find("2. ",h) = true orelse find("3. ",h) = true orelse find("4. ",h) = true orelse find("5. ",h) = true orelse find("6. ",h) = true orelse find("7. ",h) = true orelse find("8. ",h) = true orelse find("9. ",h) = true
                         then (taglist1(h)^"</li><li>"^taglist2(h))::tagmlist(t)
                         else h::tagmlist(t)
fun removelist1 [] = []
    | removelist1 (h::t) = if h = "\n"
                            then h :: t
                            else removelist1(t)
fun orderedlist1 [] = []
    | orderedlist1 (h::t) = if find("0. ",h) = true orelse find("1. ",h) = true orelse find("2. ",h) = true orelse find("3. ",h) = true orelse find("4. ",h) = true orelse find("5. ",h) = true orelse find("6. ",h) = true orelse find("7. ",h) = true orelse find("8. ",h) = true orelse find("9. ",h) = true 
                             then ["<ol>"^taglist1(h)^"<li>"]@tagmlist(taglist2(h)::t)@["</li></ol>"]@orderedlist1(removelist1(t))
                             else h::orderedlist1(t)   
(*THE ABOVE FUNCTION i.e. ORDEREDLIST1 WAS MADE FOR INDENTING ORDERED LIST BUT IT IS NOT 
WORKING PROPERLY SO I HAVE NOT CALLED IT FROM MY MAIN FUNCTION i.e. HTML_CONVERT*)
(*THE FUNCTION ABOVE IT  IS WORKING FOR ORDERED LIST BUT IT IS NOT GENERALIZED FOR ALL CASES OF 
ORDERED LIST IT WILL WORK IF THE LIST IS A SEQUENCE OF PARAGRAPHS*)


(*-----------------------------------------------------------------------------------------*)
(*---------------------------END OF "ORDERED" FUNCTIONS------------------------------------*)
(*-----------------------------------------------------------------------------------------*)

(*-----------------------------------------------------------------------------------------------------------------*)
(*FUNCTIONS TO DEAL WITH "UNORDERED LIST" AND CONVERT IT TO HTML'S RESPECTIVE TAG i.e. <UL>...<LI>TEXT</LI>...</UL>*)
(*-----------------------------------------------------------------------------------------------------------------*)

fun findhyphenpos1 [] = raise underflow 
    | findhyphenpos1 (h::t) = if (Char.compare(h,#"-"))= EQUAL   
                    then 0
                    else 1 + findhyphenpos1(t)
fun removehyphen a = String.extract(a,findhyphenpos1(String.explode(a))+1,NONE)
fun tagunlist11 [] =[]
    | tagunlist11 (h::t) = if find(" - ",h) = true 
                       then ("<li><p>"^removehyphen(h)^"</li>")::tagunlist11(t)
                       else []

fun removeunlist1 [] = [] 
    | removeunlist1 (h::t) = if find(" - ",h) = true 
                          then removeunlist1(t)
                          else h::t
fun unorderedlist1 [] =[]
    | unorderedlist1 (h::t) = if find(" - ",h) = true                           
    						 then ["<ul>"]@tagunlist11(h::t)@["</ul>"]@unorderedlist1(removeunlist1(t))
                           	 else h::unorderedlist1(t)
(*THE ABOVE FUNCTION i.e. UNORDEREDLIST1 WAS MADE FOR INDENTING UNORDERED LIST BUT IT IS NOT 
WORKING PROPERLY SO I HAVE NOT CALLED IT FROM MY MAIN FUNCTION i.e. HTML_CONVERT*)
(*THE FUNCTION BELOW IS WORKING FOR UNORDERED LIST BUT IT IS NOT GENERALIZED FOR ALL CASES OF 
UNORDERED LIST. IT WILL WORK IF THE LIST IS CONTINUOUS WITHOUT ANY PARAGRAPHS*)

fun findhyphenpos [] = raise underflow 
    | findhyphenpos (h::t) = if (Char.compare(h,#"-"))= EQUAL   
                    then 0
                    else 1 + findhyphenpos(t)
fun  tagunlist1 a =  String.substring(a,0,findhyphenpos(String.explode(a)))
fun  tagunlist2 a = String.extract(a,findhyphenpos(String.explode(a))+1,NONE)
fun  tagunlist [] = []
    | tagunlist (h::t) = if h = "\n"
                         then []
                         else if find(" - ",h) = true 
                         then (tagunlist1(h)^"</li><li>"^tagunlist2(h))::tagunlist(t)
                         else h::tagunlist(t)
fun removeunlist [] = []
    | removeunlist (h::t) = if h = "\n"
                            then h :: t
                            else removeunlist(t)
fun unorderedlist [] = []
    | unorderedlist (h::t) = if find(" - ",h) = true 
                             then ["<ul>"^tagunlist1(h)^"<li>"]@tagunlist(tagunlist2(h)::t)@["</li></ul>"]@unorderedlist(removeunlist(t))
                             else h::unorderedlist(t)   

(*----------------------------------------------------------------------------------------------*)
(*---------------------------END OF "UNORDERED LIST" FUNCTIONS----------------------------------*)
(*----------------------------------------------------------------------------------------------*)

(*-----------------------------------------------------------------------------------------------------------------*)
(*FUNCTIONS TO DEAL WITH "BOLD WORD" AND CONVERT IT TO HTML'S RESPECTIVE TAG i.e. <STRONG>.....TEXT.......</STRONG>*)
(*-----------------------------------------------------------------------------------------------------------------*)

fun findastriskpos [] = 0
	| findastriskpos (h::[]) = 0 
    | findastriskpos (h::t) = if (Char.compare(h,#"*"))= EQUAL andalso (Char.compare(hd(t),#"*")) = EQUAL   
                    then 0
                    else 1 + findastriskpos(t)
fun  tagastrisk3 a =  String.substring(a,0,findastriskpos(String.explode(a)))
fun  tagastrisk4 a =  String.extract(a,findastriskpos(String.explode(a))+2,NONE)

fun  removeastrisk a = tagastrisk3(a)^"</strong>"^tagastrisk4(a)
fun  tagastrisk1 a =  String.substring(a,0,findastriskpos(String.explode(a)))
fun  tagastrisk2 a = removeastrisk(String.extract(a,findastriskpos(String.explode(a))+2,NONE))
 

fun convertastrisk a = if find("**",a) = true
							  then	convertastrisk(tagastrisk1(a)^"<strong>"^tagastrisk2(a))
							  else  a

(*----------------------------------------------------------------------------------------------*)
(*---------------------------END OF "BOLD WORD" FUNCTIONS---------------------------------------*)
(*----------------------------------------------------------------------------------------------*)

(*--------------------------------------------------------------------------------------------------*)
(*FUNCTIONS TO DEAL WITH "UNDERLINES" AND CONVERT IT TO HTML'S RESPECTIVE TAG i.e. <U>...TEXT...</U>*)
(*--------------------------------------------------------------------------------------------------*)

fun findunderscorepos [] = 0 
    | findunderscorepos (h::t) = if (Char.compare(h,#"_"))= EQUAL    
                    then 0
                    else 1 + findunderscorepos(t)
fun  tagunderscore3 a =  String.substring(a,0,findunderscorepos(String.explode(a)))
fun  tagunderscore4 a =  String.extract(a,findunderscorepos(String.explode(a))+1,NONE)

fun  removeunderscore a = tagunderscore3(a)^"</u>"^tagunderscore4(a)
fun  tagunderscore1 a =  String.substring(a,0,findunderscorepos(String.explode(a)))
fun consunderscore1 [] = []
    |consunderscore1 (h::t) = if h = #" " orelse h = #"\t" orelse h = #"\n"
    						 then h::t
    						 else consunderscore1(t)

fun consunderscore2 [] = []
    |consunderscore2 (h::t) = if (h <> #" " andalso h <> #"\t" andalso h <> #"\n" ) andalso h <> #"_"
    						 then h::consunderscore2(t) 
    						 else if (h <> #" " andalso h <> #"\t" andalso h <> #"\n" ) andalso h = #"_"
    						 then #" "::consunderscore2(t)
    						 else []
fun  tagunderscore2 a ="</u>"^(implode(consunderscore1(explode(String.extract(a,findunderscorepos(String.explode(a))+1,NONE)))))
fun  tagunderscored a =(implode(consunderscore2(explode(String.extract(a,findunderscorepos(String.explode(a))+1,NONE)))))
 

fun convertunderscore a = if find("_",a) = true 
							  then	convertunderscore(tagunderscore1(a)^"<u>"^tagunderscored(a)^tagunderscore2(a))
							  else  a

(*----------------------------------------------------------------------------------------------*)
(*---------------------------END OF "UNDERLINE" FUNCTIONS---------------------------------------*)
(*----------------------------------------------------------------------------------------------*)

(*-----------------------------------------------------------------------------------------------------------------*)
(*FUNCTIONS TO DEAL WITH "ESCAPE SEQUENCES" FOR DETERMINING NOT TO CONVERT IT INTO HTML TAG e.g. "\_" WILL NOT ----*)
(*-----------CONVERT INTO UNDERLINE TAG OF HTML SINCE BACKSLASH "\" IS THERE---------------------------------------*)
(*-----------------------------------------------------------------------------------------------------------------*)

fun findescape (h1,h2) = if ((Char.toString(h1))^(Char.toString(h2))) = "\\\\_"
					   then true
					    else false

fun removeslashunderscore [] = []
	| removeslashunderscore (h ::[]) = [h]
	| removeslashunderscore (h::t) = if findescape(h,hd(t)) = true
									 then (#" ")::removeslashunderscore(tl(t))
									 else h :: removeslashunderscore(t)


fun convertescape a = String.implode(removeslashunderscore(String.explode(a))) 

(*-----------------------------------------------------------------------------------------------*)
(*---------------------------END OF "ESCAPE SEQUENCE" FUNCTIONS----------------------------------*)
(*-----------------------------------------------------------------------------------------------*)

(*-----------------------------------------------------------------------------------------------------------*)
(*FUNCTIONS TO DEAL WITH "ITALIC WORD" AND CONVERT IT TO HTML'S RESPECTIVE TAG i.e. <EM>.....TEXT.......</EM>*)
(*-----------------------------------------------------------------------------------------------------------*)

fun findastriskIpos [] = 0
    | findastriskIpos (h::t) = if (Char.compare(h,#"*"))= EQUAL    
                    then 0
                    else 1 + findastriskIpos(t)
fun  tagastriskI3 a =  String.substring(a,0,findastriskIpos(String.explode(a)))
fun  tagastriskI4 a =  String.extract(a,findastriskIpos(String.explode(a))+1,NONE)

fun  removeastriskI a = tagastriskI3(a)^"</em>"^tagastriskI4(a)
fun  tagastriskI1 a =  String.substring(a,0,findastriskIpos(String.explode(a)))
fun  tagastriskI2 a = removeastriskI(String.extract(a,findastriskIpos(String.explode(a))+1,NONE))
 

fun convertastriskI a = if find("*",a) = true
							  then	convertastriskI(tagastriskI1(a)^"<em>"^tagastriskI2(a))
							  else  a	

(*-----------------------------------------------------------------------------------------------*)
(*---------------------------END OF "ITALIC WORD" FUNCTIONS--------------------------------------*)
(*-----------------------------------------------------------------------------------------------*)

(*-------------------------------------------------------------------------------------------*)
(*FUNCTIONS TO DEAL WITH "HORIZONTAL LINES" AND CONVERT IT TO HTML'S RESPECTIVE TAG i.e. <HR>*)
(*-------------------------------------------------------------------------------------------*)

fun findhyphenpos [] = 0
	| findhyphenpos (h::[t1,t2]) = 0
    | findhyphenpos (h::t) = if (Char.compare(h,#"-"))= EQUAL andalso (Char.compare(hd(t),#"-"))= EQUAL andalso (Char.compare(hd(tl(t)),#"-"))= EQUAL   
                    then 0
                    else 1 + findhyphenpos(t)
fun  taghyphen1 a =  String.substring(a,0,findhyphenpos(String.explode(a)))
fun  taghyphen2 a =  String.extract(a,findhyphenpos(String.explode(a))+3,NONE)


fun converthyphen [] = []
	| converthyphen (h::t) = if find("---",h) = true
							  then	converthyphen((taghyphen1(h)^"<hr>"^taghyphen2(h))::t)
							  else  h::converthyphen(t) 

(*-----------------------------------------------------------------------------------------------*)
(*---------------------------END OF "HORIZONTAL LINE" FUNCTIONS----------------------------------*)
(*-----------------------------------------------------------------------------------------------*)

(*-----------------------------------------------------------------------------------------------------*)
(*-------------------------------------------FUNCTIONS TO DEAL WITH "BLOCK QUOTES"---------------------*)
(*-----------------------------------------------------------------------------------------------------*)
(*ASSUMING THAT THE WHENEVER ">" SYMBOL APPEARS THIS WILL TAKE THE WHOLE PARA IN BLOCK QUOTES AND IT MUST 
CONTAIN A SPACE BAR BEFORE AND AFTER IT OTHERWISE IT WILL NOT WORK  *)
	
	fun findblockpos [] = 0
		| findblockpos (h::[]) = 0
		| findblockpos (h::[t1]) = 0
		| findblockpos (h::[t1,t2]) = 0
    | findblockpos (h::t) = if (Char.compare(h,#" "))= EQUAL andalso (Char.compare(hd(t),#">"))= EQUAL  andalso (Char.compare(hd(tl(t)),#" "))= EQUAL    
                    then 0
                    else 1 + findblockpos(t)

    fun findparapos [] = 0
		| findparapos (h::[]) = 0
		| findparapos (h::[t1]) = 0
		| findparapos (h::[t1,t2]) = 0
    | findparapos (h::t) = if (Char.compare(h,#"<"))= EQUAL andalso (Char.compare(hd(t),#"/"))= EQUAL  andalso (Char.compare(hd(tl(t)),#"p"))= EQUAL    
                    then 0
                    else 1 + findparapos(t)

fun  tagblock3 a =  String.substring(a,0,findparapos(String.explode(a)))
fun  removeblock a = tagblock3(a)^" \" \n </p>"
fun  tagblock1 a =  String.substring(a,0,findblockpos(String.explode(a)))
fun  tagblock4 a =  String.extract(a,findblockpos(String.explode(a))+3,NONE)
fun  tagblock2 a = tagblock4(removeblock(a))
 
	fun convertblock [] = []
		| convertblock (h::t) = if find(" > ",h) = true
								then (tagblock1(h)^"\n\t \" "^tagblock2(h))::convertblock(t)
								else h::convertblock(t)
(*--------------------------------------------------------------------------------------------------------*)
(*---------------------------END OF "BLOCK QUOTES" FUNCTIONS----------------------------------------------*)
(*--------------------------------------------------------------------------------------------------------*)

(*-----------------------------------------------------------------------------------------------------*)
(*---------------------------------------------FUNCTIONS TO DEAL WITH "LINKS TAG "---------------------*)
(*-----------------------------------------------------------------------------------------------------*)
	fun findlinkpos [] = 0
		| findlinkpos (h::[]) = 0
		| findlinkpos (h::[t1]) = 0
		| findlinkpos (h::[t1,t2]) = 0
    | findlinkpos (h::t) = if (Char.compare(h,#"h"))= EQUAL andalso (Char.compare(hd(t),#"t"))= EQUAL  andalso (Char.compare(hd(tl(t)),#"t"))= EQUAL    
                    then 0
                    else 1 + findlinkpos(t)

fun  removelink1 [] = []
	| removelink1 (h::t) = if h = #">"
						  then []
						  else h::removelink1(t)

fun  removelink2 [] = []
	| removelink2 (h::t) = if h = #">"
						  then h::t
						  else removelink2(t)

fun  taglink1 a =  String.substring(a,0,findlinkpos(String.explode(a)))
fun  taglink2 a =  String.implode(removelink1(String.explode(String.extract(a,findlinkpos(String.explode(a)),NONE))))
fun  taglink3 a =  String.implode(removelink2(String.explode(String.extract(a,findlinkpos(String.explode(a)),NONE))))
 
	fun convertlink [] = []
		| convertlink (h::t) = if find("<http:",h) = true orelse find("<https:",h) = true
								then (taglink1(h)^"<a href=\""^taglink2(h)^"\">"^taglink2(h)^"</a>"^taglink3(h))::convertlink(t)
								else h::convertlink(t)
(*------------------------------------------------------------------------------------------------*)
(*---------------------------END OF "LINK" FUNCTIONS----------------------------------------------*)
(*------------------------------------------------------------------------------------------------*)


(*---------------------------------------------------------------------------------------------------*)
(*FUNCTIONS TO DEAL WITH "CSV SYNTAX" AND CONVERT IT TO HTML'S RESPECTIVE TAG i.e. <table>...</table>*)
(*---------------------------------------------------------------------------------------------------*)

fun findcsvpos1 [] = 0
	| findcsvpos1 (h::[]) = 1
    | findcsvpos1 (h::t) = if (Char.compare(h,#"<"))= EQUAL andalso (Char.compare(hd(t),#"<"))= EQUAL   
                    then 0
                    else 1 + findcsvpos1(t)

fun findcsvpos2 [] = 0
	| findcsvpos2 (h::[]) = 1
    | findcsvpos2 (h::t) = if (Char.compare(h,#">"))= EQUAL andalso (Char.compare(hd(t),#">"))= EQUAL   
                    then 0
                    else 1 + findcsvpos2(t)

fun removeinitials [] = []
	| removeinitials (h::t) = if (h = #" " orelse h = #"\t")
							  then removeinitials(t)
							  else h::t
fun removecsvend [] = []
	| removecsvend (h::t) = if h = #"\n"  
							  then (#" ")::removecsvend(t)
							  else h::t							  	
fun removecsv [] = []
	| removecsv (h::[]) = [h]
	| removecsv (h::t) = if h = #"<" andalso hd(t) = #"<"
							then tl(t)
							else removecsv(t)

fun removespaces [] = []
	| removespaces (h::[]) = [h]
	| removespaces (h::[t]) = if  h = #" "  andalso t = #" " 
							 then []
							 else if h <> #" " andalso t = #" "
							 then [h] 
							 else if h = #" " andalso t <> #" "
							 then [t]
							 else [h,t] 
	| removespaces (h::t) = if (h = #" " orelse h = #"\t") andalso (hd(t) = #" " orelse hd(t) = #"\t") andalso (hd(tl(t)) = #" " orelse hd(tl(t)) = #"\t")
							then removespaces(tl(t))
							else if (h = #" " orelse h = #"\t") andalso (hd(t) = #" " orelse hd(t) = #"\t") andalso (hd(tl(t)) <> #" " orelse hd(tl(t)) <> #"\t")
							then removespaces(t)
							else h::removespaces(t)

fun html_table_syntax [] = []
	| html_table_syntax (h::[]) = h::[#"<",#"/",#"T",#"D",#">",#"<",#"/",#"T",#"R",#">",#"\n",#"<",#"/",#"T",#"A",#"B",#"L",#"E",#">",#"\n"]
	| html_table_syntax (h::t) = if h = #"," orelse h = #"|"
								 then [#"<",#"/",#"T",#"D",#">",#"<",#"T",#"D",#">"]@(html_table_syntax(t))
								 else if h = #" "
								 then [#"<",#"/",#"T",#"D",#">",#"<",#"/",#"T",#"R",#">",#"\n",#"<",#"T",#"R",#">",#"<",#"T",#"D",#">"]@(html_table_syntax(t))
								 else [h]@html_table_syntax(t) 


fun  tagcsv1 a =  String.substring(a,0,findcsvpos1(String.explode(a)))
fun  tagcsvmain (a) =  String.implode(html_table_syntax(removespaces(rev(removeinitials(removecsvend(rev(removeinitials(removeinitials(removecsv(String.explode(a)))))))))))
fun  tagcsv2 a =  String.extract(a,findcsvpos2(String.explode(a))+2,NONE)
fun  tagcsv3 a =  String.substring(a,0,findcsvpos2(String.explode(a)))

fun convertcsv [] = []
	| convertcsv (h::t) = if find("<<",h) = true andalso find(">>",h) = true
							  then	convertcsv((tagcsv1(h)^"\n<CENTER><TABLE BORDER = \"1\">\n<TR><TD>"^tagcsvmain(tagcsv3(h))^"</CENTER>"^tagcsv2(h))::t)
							  else  h::convertcsv(t) 



(*-----------------------------------------------------------------------------------------------*)
(*---------------------------END OF "CSV" FUNCTIONS----------------------------------------------*)
(*-----------------------------------------------------------------------------------------------*)


(*--------------------------------------------------------------------------------------------*)
(*---------------------------BEGIN OF "MAIN" FUNCTION"----------------------------------------*)
(* BELOW IS THE MAIN FUNCTION WHICH CALL EACH INDIVIDUAL FUNCTION RECURSIVELY,DEFINED ABOVE   *)
(*--------------------------------------------------------------------------------------------*)

fun parse(filename) = write("mdtab.txt.html",convertunderscore(convertescape(convertastriskI(convertastrisk(convertstring(convertlink(convertcsv((orderedlist(convertblock((lick("mdtab.txt.html",write("mdtab.txt.html",(convert4(convertstring((unorderedlist((convert5(convert3((converthyphen(convert0("<p>>"::lick(filename,()))))))))))))))))))))))))))
(*fun give (filename:string) = write("rahul.html",implode(lick(filename)))     *)

fun convert_into_html(filename)= (writeLine("rahul.html",""),parse(filename))

(*-----------------------------------------------------------------------------------------*)
(*---------------------------END OF "MAIN" FUNCTION----------------------------------------*)
(*-----------------------------------------------------------------------------------------*)

(*SIR MY ORDERED AND UNORDERED LIST ARE NOT WOKING PROPERLY FOR SOME CASES . AND I HAVE IMPLEMENTED ONLY 
AUTOMATIC LINKS , IAM NOT ABLE TO REMOVE LABELS LIKE [JG]: AND NOT ABLE TO PROCESS IT FOR LINKS LIKE [TEXT][JG]. 
AND REST ALL THINGS ARE WORKING PROPERLY INCLUDING CSVs*)