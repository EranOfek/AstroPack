use File::Slurp;

# TESTED : handle String.toInt()
# TESTED : String.Trim()
# TESTED : handle /* comments
# TESTED : handle return
# TESTED : for statement
# TESTED : delete statement
# TESTED : handle catch and finally
# TESTED : Replace NULL with nil
# TESTED : Bugfix variable declarations (particularly when there's a comment... Also fails when there's a star
# TESTED : ++ / -- => Inc / Dec
# TESTED : handle __classid
# TESTED : handle casts
# TESTED : Handle <= / >= correctly
# TODO : Handle ternary operator
# TESTED : Handle ARRAYOFCONST
# TESTED : Handle throw
# TESTED : Handle switch
# TESTED : Variable declarations : handle int x, y, z
# TESTED : Handle += / -=
# TESTED : Handle << and >>
# TODO : Add overload to overloaded functions
# TESTED : Handle functions outside a class
# TESTED : Add ; after if then begin / end
# TESTED : Hex values 0x to $
# TESTED : handle private/public correctly
# TESTED : Application->MessageBox
# TESTED : Add all members from DFM
# TESTED : do...while
# TESTED : Add units to uses clause based on keywords in code
# TESTED : Handle destructor
# DONE : Handle c_str
# TODO : Duplicate variable names

$single_line_comment_prefix = '$$$__COMMENT__';
$multi_line_comment_prefix = '$$$__MULTICOMMENT__';


@empty_sets = ("TLocateOptions()", "TFontStyles()", "TItemStates()", "TReplaceFlags()", "TMsgDlgButtons()", "TfrxPreviewButtons()");
my $i = 0;
foreach (@empty_sets) {
	@empty_sets[$i] = quotemeta(@empty_sets[$i]);
	$i++;
}
$empty_sets = join("|", @empty_sets);
@includes_to_ignore = ("vcl", "headers");

$equality_op = qr/(== | != | < | > | <= | >=)/x;
$unary_op = qr/( ((?<! &) & (?! &)) | \* | ((?<! !) ! (?! !)) | (\+\+) | (--) | ((?<! \+) \+ (?! \+)) | ((?<! -) - (?! -)))/x;
$logical_op = qr/(\&\& | (\|\|))/x;
$value_op = qr/(\* | % | \\ | \+ | \- | & | \| | << | >> | \^)/x;

$bool = qr/(true | false)/x;
my $string_literal_prefix = '$$$__STRING__';
$string_prefix2 = quotemeta($string_literal_prefix);
$string = qr/(?: ${string_prefix2} (\d+)\$\$\$)/x;
$number = qr/(?: ((- \s*)? \d+(\.\d+)?) | (0x ([a-f] | [A-F] | [0-9])+))/x;
$literal = qr/(?: $number | $string| $bool)/x;

$string_no_markers = qr/"(([^\\"]) | (\\") | (\\\\) | (\\\w))*"/x;
$literal_no_str_markers = qr/(?: $number | $string_no_markers | $bool)/x;
							  
#$assignment_op = qr/= | \+= | -= | \*= | \/= | %= | \|= | \^= | <<= | >>=/x;
#$identifier = qr/(?<identifier> \w+($brackets)?( \s*(\.|->)(?&identifier))?)/;
$array = qr/(?<array>\[ ([^\[\]] | (?&array))*? \] )/x;
$braces = qr/(?<braces>\{ ([^\{\}] | (?&braces))*? \} )/x;
$brackets = qr/(?<brackets>\( ([^\(\)] | (?&brackets))*? \))/x;
$identifier = qr/(?<identifier> \w+(($brackets)|($array))?( \s*(\.|->)(?&identifier))?)/x;
$params = qr/(?<params> ([^\(\)] | $brackets)+)/x;
$multi_comment_marker = qr/\$\$\$__MULTICOMMENT__(\d+)\$\$\$/x;
$single_comment_marker = qr/\$\$\$__COMMENT__(\d+)\$\$\$/x;
$comment_marker = qr/(($multi_comment_marker) | ($single_comment_marker))/x;

$if_marker = '$$$____IF_A_LA_DELPHI_____$$$$$';
$elseif_marker = '$$$____ELSEIF_A_LA_DELPHI_____$$$$$';
$while_marker = '$$$____WHILE_A_LA_DELPHI_____$$$$$';
$for_marker = '$$$____FOR_A_LA_DELPHI_____$$$$$';

$if_marker_quoted = quotemeta($if_marker);
$elseif_marker_quoted = quotemeta($elseif_marker);
$while_marker_quoted = quotemeta($while_marker);
$for_marker_quoted = quotemeta($for_marker);

#$if_marker = "$$$____IF_A_LA_DELPHI_____$$$$$";
#$if_marker = "$$$____IF_A_LA_DELPHI_____$$$$$";

#$scalar_expr = qr/(?<scalar> ($identifier | $litteral) ($math_op (?&scalar)))?/x;
#$scalar_expr = qr/(($unary_op* \s* $identifier \s* $unary_op* \s*) $math_op  )/x;
#$logical_expr = qr/(($scalar_expr \s* $equality_op \s* $scalar_expr) | ($unary_op \s* $scalar_expr) | ($scalar_expr \s* $unary_op))/x;
#$ternary_operator = qr/($logical_expr \s* \? \s* $scalar_expr \s* : \s* $scalar_expr)/x;

# units to add to implementation uses clause if corresponding tokens are found
# The tokens are searched for in the Delphi code, after all the other conversions have taken place
# Tokens can contain regex patterns if need be, and any special character must be escaped
%units_to_add_by_token = (qr/\bTRect\b/ => "System.Types", qr/MB_\w+/ => "Winapi.Windows", qr/\b RefreshQuery \b \(/x => "RxDBUtils",
			qr/\b (T(DB)?Chart | T(\w+)Series) \b/x => "VCLTee.Series, VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.DBChart",
			qr/\bTRzPanel\b/ => "RzPanel", qr/\bTcxCustomGridTableView\b/ => "cxGridCustomTableView",
			qr/\bVar(IsNull|Clear)\b/ => "Variants", qr/\b(TcxGridSite|TcxCustomGridHitTest)\b/ => cxGridCustomView,
			qr/\bTDataSet\b/ => "DB", qr/\bPrinter(s)?\b/ => "Printers", qr/\b RxQuery \b/x => "RxQuery",
			);

sub getFormName {
    $h =~ /^\s*class(?:\s+\w+)?\s+(\w+)\s*:\s*public\s+(\w+)/m;
	(my $class_name, my $ancestor) = ($1, $2);
	$h =~ /^\s*extern\s+PACKAGE\s+$class_name\s+\*\s*(\w+)\s*;/m;
	return ($class_name, $ancestor, $1);
}

%escapeChars = ('\n' => "'#10'",
				'\r' => "'#13'",
				'\t' => "'#9'",
				);
				
%typeNameConversions = ("int" => "Integer",
                        "bool" => "Boolean",
						"else" => "",
						"return" => "",	
						"delete" => "",
						"AnsiString" => "String",
						"float"  => "Double",
					   );

sub convertEscapeChar {
	my $char = shift;	
	if (defined($escapeChars{$char})) {
		return $escapeChars{$char};
	}
	else {
		return $char;
	}
}

sub convertTypeName {
	my $type = shift;
	if (defined($typeNameConversions{$type})) {
		return $typeNameConversions{$type};
	}
	else {
		return $type;
	}
}

sub checkDuplicateVarNames {
	(my $variable_decls, my $var_name) = @_;
	my $varcount = 0;
	foreach my $var_decl (@$variable_decls){
		if ($var_decl =~ /^$var_name: .*/) {
			$varcount++;
		}
	}
	if ($varcount == 0) {
		return $var_name;
	}
	else {
		return $var_name. ($varcount + 1);	
	}
}

sub convertTry
{
  (my $try_body, my $catch, my $except_type, my $except_var, my $finally) = @_;
  $try_body =~ s/\s*\{\n*(.*)\}\s*/$1/s;
  $catch =~ s/\s*\{\n*(.*)\}\s*/$1/s;
  $finally =~ s/\s*\{\n*(.*)\}\s*/$1/s;
  
  $try_body = convertBlock($try_body);
  $catch = convertBlock($catch);
  $finally = convertBlock($finally);
    
  if ($except_type) {
	$catch = "except on $except_var: $except_type do begin\n${catch}end\n";
  }
  elsif ($catch) {
	$catch = "except $catch";
  }
  
  if ($finally){
	$finally = "finally\n$finally";
  }
  return "try\n$try_body$catch${finally}end;";
}

sub formatBlock{
  (my $code, my $indent) = @_;
  # Remove final ; if it's there
  $$code =~ s/;\s*$//s;

  if ($$indent eq "") {
	$$indent = " ";
  }
  # Replace braces by begin/end
  $$code =~ s/\{(.*)\}/begin${1}end/s;

  $$code = convertBlock($$code);
}

#sub formatBooleanExpression{
#  (my $expr) = @_;
#  $$expr =~ s/(.*)\|\|(.*)/($1) or ($2)/;
#}

sub convertIf{
  (my $cond, my $if_code, my $if_indent, my $if_indent2, my $elseif, my $else_code, my $else_indent, my $else_indent2) = @_;
 
	$cond =~ s/\((.*)\)/$1/s;

	my $elseifs = "";
	print $elseif;
	while ($elseif =~ /else \s* if \s* (?<cond> $brackets) \s* (?<code> $braces | (?:[^{};]+;))/xg) {
		my $elseif_cond = $+{cond};
		my $elseif_code = $+{code};
     	$elseif_cond =~ s/\((.*)\)/$1/s;

		formatBlock(\$elseif_code);
		#print $elseif_code;
		$elseifs .= "\n" .$elseif_marker . " $elseif_cond then $elseif_code";
	}
		

#  if ($cond =~ /^(\s* \( \s* ([\s\w.]|->)+ \s*) (\) \s*) $/x) {
#	$cond = $1 . " != 0" . $3;
#  }
 
  formatBlock(\$if_code, \$if_indent);
  formatBlock(\$else_code, \$else_indent2); 
  
  if ($else_code){	
    return "\n". $if_marker . " $cond then$if_indent$if_code$if_indent2$elseifs${else_indent}else${else_indent2}$else_code;";
  } 
  else {  
	return "\n". $if_marker . " $cond then$if_indent$if_code$elseifs;$if_indent2";
  }
}
	
sub convertWhile{
  (my $cond, my $indent, my $code) = @_;
  
#  if ($cond =~ /^(\s* \( \s* ([\s\w.]|->)+ \s*) (\) \s*) $/x) {
#	$cond = $1 . " != 0" . $3;
#  }

  formatBlock(\$code, \$indent);
  return $while_marker . " " . $cond ." do".$indent.$code.";";
}

sub convertDoWhile{
  (my $cond, my $indent, my $indent2, my $code) = @_;

#  if ($cond =~ /^(\s* \( \s* ([\s\w.]|->)+ \s*) (\) \s*) $/x) {
#	$cond = $1 . " != 0" . $3;
#  }

  formatBlock(\$code, \$indent);
  return "repeat ".$indent.$code.$indent2."until not $cond;";
}

sub convertSwitch{
  (my $expr, my $indent, my $code) = @_;
  
  my $case_blocks = "";
  my $default_block = "";
  
  # Handle default case
  if ($code =~ /\b default \s* : \{? (?<block> [\s\S]+) \}/x) {
	my $block = $+{block};
	$block = 'begin ' . convertBlock($block) . ' end;';
	$default_block = "else $block";
  }
   
  if ($expr =~ /DBGrid1->SelectedIndex/) {
	#print $code;
  }
  while ($code =~ /\b (?<cases> case \s+ \w+ \s* : \s* (case \s+ \w+ \s* : \s*)*) \{? (?<block> ([^\{\}] | $braces)+?) ((break) | \})/xg) 
  {
    my $cases = $+{cases};	
	my @cases = ();
	while ($cases =~ /\b case \s* (\w+) \s* :/xg) {
		push(@cases, $1);
	}
	$cases = join(', ', @cases) . ' :';
		
	my $block = $+{block};
	

	$block = ' begin ' . convertBlock($block) . " end;\n";
	$case_blocks .= $cases . $block;
  }
  #print $case_blocks;
  return "case " . $expr ." of\n".$indent.$case_blocks.$default_block . "\nend;";
}

sub convertFor{
	(my $cond, my $indent, my $code) = @_;

	formatBlock(\$code, \$indent);
  
	my $initial_for_loop = $cond;
	my @statements = split(/\s*;\s*/, $initial_for_loop =~ s/\s*\((.*)\)\s*/$1/sr);
	my $init = shift(@statements);
	my $cond = shift(@statements);
	my $increment = shift(@statements);	
	my $loop = "";
	if ($increment =~ /\s*(\w+)\s*([+-]{2})/) {
		my $var_name = $1;
		my $inc = $2;
		$init =~ s/$var_name \s* = \s* (.*) \s*/$var_name = $1/x;
		if ($inc eq "++") {
			$increment = "to";
		}
		else {
			$increment = "downto";
		}
		if ($cond =~ /$var_name \s* ([<>]=?) \s* (.+) \s*/xs) {
		    my $upper_bound = $2;
			if ($1 eq "<") {
				# strictly inferior...
				if ($upper_bound + 0 ne $upper_bound) {
					$upper_bound = $2 . ' - 1';
				}
				else {
					$upper_bound = $2 - 1;
				}
			}
			elsif ($1 eq ">") {
				# strictly inferior...
				if (($upper_bound + 0) ne $upper_bound) {
					$upper_bound = $2 . ' + 1';
				}
				else {
					$upper_bound = $2 + 1;
				}
			}
			$loop = "$init $increment $upper_bound";
		}
		else{
			# More complex for loops aren't handled, so we just return the original statement
			$loop = $initial_for_loop
		}		
	}
	else {
		# More complex for loops aren't handled, so we just return the original statement
		$loop = $initial_for_loop
	}

  return  $for_marker . " " . $loop ." do".$indent.$code.";";
}	

sub convertBlock{
	my $code = shift;
	# Handle if (...)
	$code =~ s/(?<! \belse) \s+ if \s* (?<cond> $brackets) (?<if_indent> \s*) (?! \s*then\b)
							  (?<if_code> ($braces | (?:[^{};]+;))) (?<if_indent2> \h*\n*)
							  (?<elseif> (\s* else \s* if \s* (?<cond> $brackets) \s* ($braces | (?:[^{};]+;)))*)
							  (?<else> (?<else_indent> \h*) else (?<else_indent2> \s*) (?! if\b)
								(?<else_code> ($braces | (?:[^{};]+;))))?
				  /convertIf($+{"cond"}, $+{"if_code"}, $+{"if_indent"}, $+{"if_indent2"}, $+{elseif}, $+{"else_code"}, $+{"else_indent"}, $+{"else_indent2"})/gxe;	

	# Handle while (...)
	$code =~ s/\b while \s* (?<cond> $brackets)(?<indent> \s*) (?! \s*do\b)
							  (?<code> ($braces | (?:[^{};]+;)))/convertWhile($+{"cond"}, $+{"indent"}, $+{"code"})/gxe;	
	
	# Handle do / while (...)
	$code =~ s/\b do \s* (?<indent> \s*) (?<code> $braces)
			   (?<indent2> \s*) while \s* (?<cond> $brackets) \s* ;
			  /convertDoWhile($+{"cond"}, $+{"indent"}, $+{"indent2"}, $+{"code"})/gxe;	

	# Handle for (;;)
	$code =~ s/\b for \s* (?<cond> $brackets)(?<indent> \s*)(?! \s*do\b) 
							  (?<code> ($braces | (?:[^{};]+;)))/convertFor($+{"cond"}, $+{"indent"}, $+{"code"})/gxe;	
	
	
	# Handle switch 
	$code =~ s/\b switch \s* (?<expr> $brackets)(?<indent> \s*)(?! \s*of\b) 
							  (?<code> ($braces | (?:[^{};]+;)))/convertSwitch($+{"expr"}, $+{"indent"}, $+{"code"})/gxe;	
	
		
	# Handle try/catch/__finally
	$code =~ s/\b try \s* (?<try_body> $braces)
					(?: \s* catch \s* \( \s* (?:(?:(?<except_type> \w+) [\s\*\&]+ (?<except_var> \w+)) | \.\.\.) \s* \) \s* (?<catch> $braces))?
					(?: \s* __finally \s* (?<finally> $braces))?
				   /convertTry($+{try_body}, $+{catch}, $+{except_type}, $+{except_var}, $+{finally})/gxe;		

   return $code;
}

sub convertCasts{
	my $code = shift;
	
	# Convert casts
	$code =~ s/\( \s* (\w+) \s* \*? \s* \) \s* ($brackets | $identifier | $literal) 
				   /convertTypeName($1) . "(" . convertCasts($2) .")"
				   /xge;
	return $code;
}

sub replaceArrayOfConst{
    "[" . removeBrackets(shift) . "]";
}

sub convertString{
	(my $cpp_string) = @_;
	my $pas_string = $cpp_string;
	$pas_string =~ s/'/''/g;
	$pas_string =~ s/\\"/"/g;
	$pas_string =~ s/(\\[a-z])/convertEscapeChar($1)/eg;
	$pas_string =~ s/\\\\/\\/g;
	#print "$cpp_string => $pas_string\n";
	return $pas_string;
}

sub removeBrackets{
	shift =~ s/\( (.*) \)/$1/sxr;
}

sub convertFunctionImpl {
    my $meth_impl = shift;
	my $init_statement = shift;
	my $final_statement = shift;

	if ($init_statement) {
		$init_statement = "\n  " . $init_statement;
	}
	if ($final_statement) {
		$final_statement = "  " . $final_statement . "\n";
	}
	
	#my $single_line_comment_prefix = '$$$__COMMENT__';
	#my $multi_line_comment_prefix = '$$$__MULTICOMMENT__';
	#my $comment_number = 0;	
	
	# Store comments in array and remove them from string
	#my @single_line_comments = ();
	#while ($meth_impl =~ s! //(.*) $ ! $single_line_comment_prefix . $comment_number . '$$$' !mxe) {
	#    #print "$current_method_name : SINGLE".$comment_number."\n";
	#	$single_line_comments[$comment_number++] = $1;		
	#}
	#my @multi_line_comments = ();
	#$comment_number = 0;
	#while ($meth_impl =~ s! /\* (.*?) \*/ ! $multi_line_comment_prefix . $comment_number . '$$$' !sxe) {
		#print "$current_method_name : MULTI".$comment_number."\n";
	#	$multi_line_comments[$comment_number++] = $1;
	#}
	
	my $string_number = 0;	
	my @string_literals = ();
	while ($meth_impl =~ s! (?: L \s*)? "((?: [^\\"] | (\\") | (\\\\) | (\\([a-z]|[A-Z])))*?)" 
						  ! $string_literal_prefix . $string_number . '$$$' 
						  !mxe) 
	{
	    $string_literals[$string_number++] = $1;		
	}
	
	$meth_impl =~ s/(\s+)::(\w+)/$1$2/g;
    $meth_impl =~ s/(\w+)::(\w+)/$1.$2/g;
    
    # Parse code section
	# Get variables...	
	my @variable_decls = ();
	while ($meth_impl =~ /(?<previous> (?<separator> ; | \{ | \}) \s* ($comment_marker)* \s* (?<for> \s* for \s* \( \s*)? \s* ($comment_marker)*)
							(?<initlines> \n*) (?<indent> \s*?) \b 
							(?<type_name> [\w.]+) (?<modifiers> \s* (?:\*|\s) \s*) 
							
							# Get list of variable names, with or without initialization statements
							(?<var_name> \w+ 
								(?:\s* = \s*  (?: [^;,] | $brackets)+ \s*)? 
								(?: \s* , \s* (?&var_name))?
							) 
							
							\s* ; \s*? (?<extralines>\n*) /xg) {
	  my $type_name = convertTypeName($+{"type_name"});
	  if (not $type_name eq "") {
	    my $for = $+{for};
	    my $indent = $+{"indent"};
		my $initlines = $+{"extralines"};
		my $extralines = $+{"extralines"};
		my $previous = $+{previous};
		my $start_pos = @-[0];
	    my $full_match = $&;
		my $full_match_len = length($full_match);
	    my $type_decl = $+{"type_name"}.$+{"modifiers"};
	    my $list_var_names = $+{"var_name"};
		my $terminator = $+{"term"};
		my @variable_inits = ();
	    
		# Handle multiple declarations separated by commas
		while ($list_var_names =~ s/(?<var_name> \w+)															
								(?: \s* = \s*  (?<init> (?: [^\(;,] | $brackets)+) \s*)? 
								//x)
		{
			my $init = $+{"init"};
			my $var = $+{"var_name"};
			my $orig_var = $var;
			
			if ($var eq "fmTarifsClientsOperations") {
				#print $full_match;
			}
		
		#foreach my $var (split(/\s*,\s*/, $orig_var_name)) {
			# The variable may be initialized
		#	my $init = "";
		#	if ($var =~ /(\w+) \s* = \s* ([^,;=\s]+)/x){
		#	    $var = $1;
		#		$init = $2;
		#	}
			
			# Rename duplicate variable names that were scoped in a for/while/do loop, since in Delphi they conflict
			$var = checkDuplicateVarNames(\@variable_decls, $var);
			
			if ($var ne $orig_var) {
				my $braces_begin;
				if ($for) {
					$braces_begin = index($meth_impl, "{", $start_pos);				
				}
				else {
					$braces_begin = rindex($meth_impl, "{", $start_pos);
				}				
				
				my $braces_depth = 0;
				my $braces_end = 0;
				for (my $i = $braces_begin+1; $i < length($meth_impl); $i++) {
					my $current_char = substr($meth_impl, $i, 1);
					if ($current_char eq "{") {
						$braces_depth++;
					}
					elsif ($current_char eq "}") {
						if ($braces_depth == 0) {
							$braces_end = $i -1;
							#$scope = substr($meth_impl, $brace_begin+1, $i -1);
							last;
						}
						$braces_depth--;
					}
				}
				#print substr($meth_impl, $braces_begin +1, 10). "\n";
				#print "$full_match_len\n";
				
				$full_match_len += length($var) - length($orig_var); #-= length(substr($meth_impl, $braces_begin +1, $braces_end));
				substr($meth_impl, $braces_begin +1, $braces_end) =~ s/\b $orig_var \b/$var/xg; 				
				#$full_match_len += length(substr($meth_impl, $braces_begin +1, $braces_end));
				
				#print "$full_match_len\n";
				
				#print substr($meth_impl, $braces_begin +1, $braces_end);
			}
			
			if (not $init eq ""){
				push(@variable_inits, "$indent$var = $init;");
			}
			push(@variable_decls, "$var: $type_name;");
		}
		if (not $initlines eq "") {
			$initlines = "\n";
		}
		if (@variable_inits) {
			if (not $extralines eq "") {
				$extralines = "\n";
			}
		}
		else {
			$extralines = "";
		}
		substr($meth_impl, $start_pos, $full_match_len) = $previous . $initlines.join("\n", @variable_inits).$extralines;				
	  }
	}
	
	$| = 1;

	$meth_impl = convertCasts($meth_impl);
	$meth_impl = convertBlock($meth_impl);
	
	# Replace markers by if / while / for tokens
	$meth_impl =~ s/$if_marker_quoted/if/g;
	$meth_impl =~ s/$elseif_marker_quoted/else if/g;
	$meth_impl =~ s/$while_marker_quoted/while/g;
	$meth_impl =~ s/$for_marker_quoted/for/g;
	
	$value_expr_part = qr/($unary_op \s*)? ($brackets | $identifier | $literal)/x;
    $value_expr = qr/$value_expr_part (\s* $value_op \s* $value_expr_part)*/x;

	$meth_impl =~ s/(?<expr> $value_expr \s* $equality_op \s* $value_expr) 
				   /($+{expr})/xg;
		
	# Remove __classid
    $meth_impl =~ s/\b __classid $brackets/removeBrackets($+{brackets})/gex;

	# Change delete to .Free
    $meth_impl =~ s/\bdelete\s+(\w+)\s*;/$1.Free;/g;

	# Convert return to Exit
    $meth_impl =~ s/\breturn \s* ($identifier)?/ defined($1) ? "Exit($1)" : Exit/gxe;
	
	# Convert throw to raise
    $meth_impl =~ s/\bthrow\s*Exception\s*\(/raise Exception.Create(/g;
    $meth_impl =~ s/\bthrow\b/raise/g;

	# Remove ; before else
	#$meth_impl =~ s/;(\s+else\b)/$1/g;
		
	# Change new to Create
	$meth_impl =~ s/\bnew\b\s+(\w+)/$1.Create/g;
	
	# Change this to Self
	$meth_impl =~ s/\bthis\b/Self/g;
	
    # Replace NULL with nil
	$meth_impl =~ s/\bNULL\b/nil/g;
	
	# Translate Hex literals
	$meth_impl =~ s/\b 0x (\d+)/\$$1/xg;
	
	# Change ++ / -- to Inc / Dec
	$meth_impl =~ s/\b($identifier)\s*\+\+/Inc($1)/gx;
	$meth_impl =~ s/\b($identifier)\s*--/Dec($1)/gx;
		
	# Change ! signs
	$meth_impl =~ s/!=/<>/xg;
    $meth_impl =~ s/!/ not /xg;
	
	# Change << and >> operators
	$meth_impl =~ s/<< \s* (?: \w+\.)? (\w+)/+ [$1]/xg;
	$meth_impl =~ s/>> \s* (?: \w+\.)? (\w+)/- [$1]/xg;
	$meth_impl =~ s/\b $empty_sets/[]/xg;	
	
	# Change += and -= operators
	$meth_impl =~ s/($identifier) \s* \+= \s* (?<val>[^;]+)/$1 = $1 \+ $+{val}/xg;
	$meth_impl =~ s/($identifier) \s* -= \s* (?<val>[^;]+)/$1 = $1 - $+{val}/xg;

	# Change ExtBcd_C and RoundPlacesBcd_C to ExtBcd and RoundPlacesBcd
	$meth_impl =~ s/\b ExtBcd_C \b/ExtBcd/xg;
	$meth_impl =~ s/\b RoundPlacesBcd_C \b/RoundPlacesBcd/xg;
	
	# Change Contains to "in"
	$meth_impl =~ s/($identifier) \s* \. \s* Contains\( $params \)/$+{params} in $1/xg;
	
	# Change TDateTime(...) to EncodeDate(...)
	if ($meth_impl =~ s/\b TDateTime \s* \( \s* $params \s* \)/EncodeDate($1)/xg){
		$includes_to_add{"System.Sysutils"} = 1;
	}
	# Change .Trim(), .ToInt(), ToIntDef() and .SubString()
	if ($meth_impl =~ s/($identifier) \s* \. \s* (?<func> (Ansi)? (UpperCase|LowerCase|Trim|TrimRight|TrimLeft)) \s* \(\s*\)/$+{func}($1)/xg) {
		$includes_to_add{"System.Sysutils"} = 1;
	}
	if ($meth_impl =~ s/($identifier | $brackets) \s* \. \s* c_str\(\s*\)/PChar($1)/xg){
		$includes_to_add{"System.Sysutils"} = 1;
	}
	if ($meth_impl =~ s/($identifier) \s* \. \s* ToInt\(\s*\)/StrToInt($1)/xg){
		$includes_to_add{"System.Sysutils"} = 1;
	}
	if ($meth_impl =~ s/\b String \s* ($brackets) /IntToStr$1/xg){
		$includes_to_add{"System.Sysutils"} = 1;
	}
	if ($meth_impl =~ s/($identifier) \s* \. \s* ToIntDef\( $params \)/StrToIntDef($1, $+{params})/xg){
		$includes_to_add{"System.Sysutils"} = 1;
	}
	if ($meth_impl =~ s/($identifier) \s* \. \s* SubString\( $params \)/Copy($1, $+{params})/xg){
		$includes_to_add{"System.Sysutils"} = 1;
	}
	if ($meth_impl =~ s/($identifier) \s* \. \s* (?:Ansi)?Pos \( $params \)/Pos($+{params}, $1)/xg){
		$includes_to_add{"System.Sysutils"} = 1;
	}
	if ($meth_impl =~ s/($identifier) \s* \. \s* FormatString\( $params \)/FormatDateTime($+{params}, $1)/xg){
		$includes_to_add{"System.Sysutils"} = 1;
	}
		
	# Change TDateTime.CurrentDate to Date
	$meth_impl =~ s/\b TDateTime [\s()]* \. \s* CurrentDate \b/Date/gx;

	# Change = signs
	$meth_impl =~ s/(?<! [<>=]) = (?! =)/:=/xg;
	$meth_impl =~ s/==/=/xg;
	
	# Change || operator
	$meth_impl =~ s/\|\|/ or /xg;

	# Change && operator
	$meth_impl =~ s/&&/ and /xg;
	

	$meth_impl =~ s/->/./g;
	
	# Replace ARRAYOFCONST
	$meth_impl =~ s/\b ARRAYOFCONST ($brackets)/replaceArrayOfConst($1)/xge;
	
	# Replace string markers by their values
	my $string_prefix2 = quotemeta($string_literal_prefix);
    $meth_impl =~ s! ${string_prefix2} (\d+)\$\$\$ ! "'" . convertString($string_literals[$1]) . "'" !gxe;
	
	# Change quotes
	#$meth_impl_copy = $meth_impl;
	#$prev_offset = 0;
	#while ($meth_impl_copy =~ /(?<!\\)"(.*?)(?<!\\)"/) {
	#    my $match_len = length($&);
#		my $offset = @-[0];
#		
#		my $pas_string = $1;
#		$pas_string =~ s/'/''/g;
#	    $pas_string =~ s/\\"/"/g;
##	$pas_string =~ s/(\\[a-z])/convertEscapeChar($1)/eg;
	#	substr($meth_impl, $prev_offset + $offset, $match_len) = "'$pas_string'";	
	#	substr($meth_impl_copy, 0, $offset + $match_len) = "";	
	#	$prev_offset = $prev_offset + $offset + $match_len + (length($pas_string) + 2 - $match_len); 
	#}   
	
	my $vars = @variable_decls? "var\n"
                               ."  ". join("\n  ", @variable_decls) . "\n" : "";

	# Remove surrounding braces and insert initial statement if defined
	$meth_impl =~ s/^(\s*) \{ (.*?) (\s*) \}(\s*) $/ 
				(defined($1)? $1 : " ") . "begin " . $init_statement. $2 . (defined($3)? $3 : " ") . $final_statement. "end" . $4 /xes;

	# Change braces to begin / end (in case any are left)
	$meth_impl =~ s/(\w)?\{(\w)?/ (defined($1)?"$1 ":'') . 'begin' . (defined($2)?" $2":'')/xge; 
	$meth_impl =~ s/(\w)?\}(\w)?/(defined($1)?"$1 ":''). 'end' . (defined($2)?" $2":'')/xge; 
	
	return $vars . $meth_impl . ";"
}

sub parseParamList{
    my $param_list = shift;
	$param_list =~ s/\s*\( (.*) \)\s*/$1/xs;
    my @params = ();
	my @test = split(/,/, $param_list);
	foreach my $param (split(/,/, $param_list)) {
		#print "$param\n";
	    my $var = ($param =~ /&/)? "var ": "";
		my @param_parts = ();
		my $modifier = "";
		foreach my $part (split(/(\s|\*|&)+/, $param)){
		  if ($part =~ /\s*([^*&]+)\s*/) {
		    $part = $1;
		    if (not $part =~ /\s+/) {
				push(@param_parts, $part);
			}
		  }
		}		
		if (@param_parts == 3) {
		    $modifier = shift @param_parts;
			if (not $modifier eq "") {
			  $modifier = $modifier . " ";
			}
		}
		my $param_type = convertTypeName(@param_parts[0]);
		my $ret = "$var$modifier@param_parts[1]: $param_type";
		push(@params, (($ret =~ s/\b var \s* const \b/var/xgr) =~ s/::/./gr));	   
	}
	return @params? join("; ", @params) : "";
}

sub getMethods{
  my @methods = ();
  while ($cpp =~ /^\s* (?<ret_value>\w+)? \s*\*?\s* (__fastcall)? \s+ ((?<class> \w+)\s*::\s*)? (?<tilde> ~?) (?<meth_name> \w+) \s*
                 \((?<param_list> [^\(\)]* ) \) \s* 
				 (?<ancestor_call> :\s*\w+ \( \s*\w+\s* \) \s*)?
				 (?<meth_impl>
				          (?<code_in_braces>\{ ([^\{\}]| (?&code_in_braces))+? \})
				  )
				 /xgm) 
  { 
    my $tilde = $+{tilde};
    my $ancestor_call = $+{"ancestor_call"};
    my $meth_name = $+{"meth_name"};
    my $param_list = $+{"param_list"};
	my $ret_value = $+{"ret_value"};
	my $meth_impl = $+{"meth_impl"};
    my $func_or_proc = "function";
	my $inherited = "";
	my $class = $+{class};
	if ($class) {
		$class .= ".";
	}
	$current_method_name = $+{"meth_name"};
	my $inherited_destructor = "";
	if ($tilde) {
		$func_or_proc = "destructor";
		$ret_value = "";
		$current_method_name = 'Destroy';
		$inherited_destructor = "inherited;";
	}
	elsif (not $ancestor_call eq "") {
		$func_or_proc = "constructor";
		$inherited = "inherited;";
		$ret_value = "";
		$current_method_name = 'Create';
	}
	elsif ($ret_value eq "void") {
		$func_or_proc = "procedure";
		$ret_value = "";
	}   	
	else {
		$ret_value = ": " . convertTypeName($ret_value);
	}
	my $params = parseParamList($param_list);
	my $meth_decl = $func_or_proc . " " . $class . $current_method_name . "($params)" . $ret_value . ";";
	
	$meth_impl = convertFunctionImpl($meth_impl, $inherited, $inherited_destructor);	
    push(@methods, $meth_decl . "\n" . $meth_impl);
  }
  return @methods;
}

$method_separator = "";

sub getFuncOrProc{
	(my $meth_name, my $class_name, my $ret_type) = @_;
	my $func_or_proc = "";

	if ($$meth_name eq $class_name) {
		#print $$meth_name . " / " . $class_name;
		$func_or_proc = "constructor";		
		$$ret_type = "";
		$$meth_name = "Create";
	}
	elsif ($$ret_type eq "void") {
		$func_or_proc = "procedure";
		$$ret_type = "";
	}
	else {
		$func_or_proc = "function";
		$$ret_type = " : " . convertTypeName($$ret_type);
	}	
	
	return $func_or_proc;	
}

sub getSectionOfClass{
	(my $class_name, my $declarations) = @_;
	my $result = "$section_name\n";	
	
	#print "test".$declarations;
	# Extract fields
	while ($declarations =~ s/(?<indent> \h*) (?<type>\w+) (?<name> (\s|\*|\&)+ \w+ (\s* , \s* (?&name))?)\s*;(?<indent2> \h*\n*)//x) {
	    my $indent = $+{indent};
		my $indent2 = $+{indent2};
		my $type = $+{type};
	    my @names = split(/\s*,\s*/, $+{name});
		foreach my $name (@names){
			$name =~ s/[\s\*\&]* (\w+)/$1/x;			
		}
		$result .= $indent . join(", ", @names). ": ". convertTypeName($+{type}) . ";". $indent2;		
	}

	# Extract properties
	#print "trying properties\n";
	while ($declarations =~ s/(?<indent> \h*) __property \s+ (?<type>\w+) (\s|\*|\&)+ (?<name> \w+)\s*
							= \s* \{ \s* (read \s* = \s* (?<read> \w+))? \s* ,? \s* (write \s* = \s* (?<write> \w+))? \s* \} \s*
							;(?<indent2> \h*\n*)//x) {
		my $indent = $+{indent};
		my $indent2 = $+{indent2};
		my $type = $+{type};
		my $read = $+{read};
		if ($read) {
			$read = " read " . $read;
		}
		my $write = $+{write};
		if ($write) {
			$write = " write " . $write;
		}
		my $name = $+{name};
		$name =~ s/(\s\*\&)* (\w+)/$2/x;
		$result .= $indent . "property ". $name. ": ". convertTypeName($type) . $read . $write .  ";". $indent2;
	}
	
	# Extract methods
	my @methods = ();
	while ($declarations =~ s/(?<indent> \h*) (?:(?<ret_val> \w+) [\s\*\&]*)? (?<modifier> __fastcall)?	 \s* (?<tilde> ~?) (?<meth_name> \w+) (?<param_list> $brackets) \s* ;//x) {
		my $ret_val = $+{ret_val};
		my $meth_name = $+{meth_name};
		my $tilde = $+{tilde};
		my $func_or_proc = getFuncOrProc(\$meth_name, $class_name, \$ret_val);
		if ($tilde) {
			$func_or_proc = "destructor";
			$ret_val = "";
			$meth_name = 'Destroy';			
		}
		my $modifiers = (($func_or_proc eq "destructor") or($func_or_proc eq "constructor")) ? "override;": "";

		my $params = parseParamList($+{param_list});
		$result .= $+{indent} . $func_or_proc . " " . $meth_name . "($params)" . $ret_val . "; $modifiers\n";
	}
		
	#print "finished methods\n";
	return $result;	
}

sub getIncludes {
	(my $declarations, my $extra_includes, my $reference_includes) = @_;
	my %includes = ();
	while ($declarations =~ s/^\#include \s+ (?: \" | \<) (\w+) (?: \.h | \.hpp) (?: \" | \>) \h*$//xm) {
		if (not grep {$_ =~ /$1/i} @includes_to_ignore) {
			$includes{$1} = 1;
		}
	}
	if ($extra_includes) {
		foreach my $inc (keys %$extra_includes) {
			$includes{$inc} = 1;
		}
	}
	
	if ($reference_includes) {
		foreach my $inc (keys %$reference_includes) {
			delete $includes{$inc};
		}
	}

	return \%includes;	
}

sub convertSectionName{    
	my $section = shift;
	if ($section eq "__published") {
		return "";
	}
	else {
		return $section;
	}
}

sub setCommentMarkers{
    (my $code) = @_;
	
	while ($code =~ s! //(.*) $ ! $single_line_comment_prefix . $comment_number . '$$$' !mxe) {
		$single_line_comments[$comment_number++] = $1;		
	}
	while ($code =~ s! /\* (.*?) \*/ ! $multi_line_comment_prefix . $comment_number . '$$$' !sxe) {
		$multi_line_comments[$comment_number++] = $1;
	}
	return $code;
}

sub removeSurroundingBraces {	
	return shift =~ s/\s* \{ ([\s\S]*) \} \s*/$1/rx;
}

sub replaceCommentMarkers{
	# Replace comment markers by their values
	(my $code) = @_;
	my $multi_line_comment_prefix2 = quotemeta($multi_line_comment_prefix);
    my $single_line_comment_prefix2 = quotemeta($single_line_comment_prefix);
	$code =~ s! ${multi_line_comment_prefix2} (\d+)\$\$\$ ! '(*' . $multi_line_comments[$1] . '*)' !gxe;
	$code =~ s! ${single_line_comment_prefix2} (\d+)\$\$\$ ! '//' . $single_line_comments[$1] !gxe;
	return $code;
}

sub getInterfaceSection{
	my $interface = "";
	
	# Remove comments
	$h =~ s! //(.*) $ !!mxg;
	$h =~ s! /\* (.*?) \*/ !!sxg;

	# Get enums
	while ($h =~ s/\b enum \s+ (?<enum_name> \w+) \s* (?<enum_members> $braces)//x) {
		my $enum_name = $+{enum_name};
		my $enum_members = $+{enum_members} =~ s/\{([^\}]*)\}/$1/r;
		$interface .= $enum_name . " = ($enum_members);\n\n";
	}	
	
	#use re 'debug';
	# Get classes
	while ($h =~ /class \s+ (?<class_name> \w+) \s* (: (\s* \w*)? \s* (?<ancestor> \w+))? \s* \{/mxg) {
		my $braces_depth = 0;
		my $start_pos = $-[0];
		my $braces_begin = $+[0] +1;
		my $braces_end = 0;
		for (my $i = $braces_begin; $i < length($h); $i++) {
			my $current_char = substr($h, $i, 1);
			if ($current_char eq "{") {
				$braces_depth++;
			}
			elsif ($current_char eq "}") {
				if ($braces_depth == 0) {
					$braces_end = $i -1;
					#$scope = substr($meth_impl, $brace_begin+1, $i -1);
					last;
				}
				$braces_depth--;			
			}
		}
		
		my $class_body = substr($h, $braces_begin , $braces_end - $braces_begin +1);
		substr($h, $start_pos , $braces_end - $start_pos) = "";
	
	#$braces = qr/(?<braces>\{ ([^\{\}]* | (?&braces))*? \} )/x;
	#if ($h =~ s/class \s+ (?<class_name> \w+) \s* (\: (\s* \w*)? \s* (?<ancestor> \w+))? \s* $braces//x) {
	#	print "hi\n";
	#}
	#while ($h =~ s/class \s+ (?<class_name> \w+) \s* (\: (\s* \w*)? \s* (?<ancestor> \w+))? \s* $braces//x) {
	#while ($h =~ s/class \s+ (?<class_name> \w+) \s* (\: (\s* \w*)? \s* (?<ancestor> \w+))? \s* \{ (?<class_body>  [\s\S]+ ) \} \s*;//xm) {
		#my $class_body = removeSurroundingBraces($+{braces});
#		my $class_body = $+{class_body};
#		print "vergvergv $class_body \n";

		my $ancestor = $+{ancestor};
		my $class_name = $+{class_name};
		my $declarations = "";		

		my %sectionEnds = ();
		my %sectionBegins = ();
		my @sections = ();
		while ($class_body =~ /^\h* (?<section> \w+ ) \s* :\h* $/xgm) {
		    push (@sections, $+{section});			
			$sectionEnds{$+{section}} = @+[0];
			$sectionBegins{$+{section}} = @-[0];
		}
		for (my $i = 0; $i < @sections; $i++) {
		    my $posBegin = $sectionEnds{@sections[$i]};		
			#print $posBegin."\n";
			my $section_content = "";
			if ($i == @sections-1) { #last element
				$section_content = substr($class_body, $posBegin+1);
			}
			else {
			    #if ($i > 0) {print $sectionBegins{@sections[$i+1]};
				#print substr($class_body, $posBegin+1, $sectionBegins{@sections[$i+1]} - $posBegin-1);}
				$section_content = substr($class_body, $posBegin+1, $sectionBegins{@sections[$i+1]} - $posBegin-1);
			}
			$declarations .= convertSectionName(@sections[$i]) . "\n";
			$declarations .= getSectionOfClass($class_name, $section_content);
		}
		
		
		#@sections = $class_body =~ /^\h* (?<section> \w+ ) \s* :\h* $/xgm;
		#foreach $section (@sections){
		#    $last_index = 0;
		#	
	#		$index = index $declarations, $section;
	#		$declarations .= $+{indent} . convertSectionName($section) . "\n";
	#		pos($declarations
	#		index $declarations,  $+[0]
	#		$declarations .= getSectionOfClass($class_name, $+{declarations});		
	#	}
	#	while ($class_body =~ /\n(?<indent> \h*) (?<section> \w+) \s* :\h*\n (?<declarations> [\s\S]*?) ((\n\h* \w+ \s* : \h* \n) | $)/xg) {
	#		$declarations .= $+{indent} . convertSectionName($+{section}) . "\n";
	#		$declarations .= getSectionOfClass($class_name, $+{declarations});
	#	}
		#print "finished sections\n";
		$interface .= $class_name . " = class($ancestor)\n$declarations\nend;\n\n";
	}
	#no re  'debug';
	#print "finished classes\n";
	# Get functions outside a class
	while ($h =~ s/(?<ret_val> \w+) [\s\*\&]* (?<modifier> \w*) \s* (?<meth_name> \w+) (?<param_list> $brackets) \s* ;//x) {		 
		my $ret_val = $+{ret_val};
		my $meth_name = $+{meth_name};
		if ($meth_name eq "ZeroPad") {
			#print "$meth_name, $class_name, $ret_val $func_or_proc \n";
		}
		my $func_or_proc = getFuncOrProc(\$meth_name, $class_name, \$ret_val);
		if ($meth_name eq "ZeroPad") {
			#print "$meth_name, $class_name, $ret_val $func_or_proc \n";
		}
		my $params = parseParamList($+{param_list});
		$interface .= $func_or_proc . " " . $meth_name . "($params)" . $ret_val . ";\n";
	}
	
	#print "finished functions\n";
	# Get variables outside a class
	#while ($h =~ s/(?<type>\w+) (?<name> (\s\*\&)+ \w+) \s* ;//x) {
	#	print "tsrgserg\n";
	my $vars = "";
	while ($h =~ s/(?<indent> \h*) (?<modifier> \w+\s+)? (?<type>\w+) (?<name> (\s|\*|\&)+ \w+ (\s* , \s* (?&name))?)\s*;(?<indent2> \h*\n*)//x) {		 		
		my $indent = $+{indent};
		my $indent2 = $+{indent2};
		my $type = $+{type};
		my @names = split(/\s*,\s*/, $+{name});
		foreach my $name (@names){
			$name =~ s/(\s|\*|\&)* (\w+)/$2/x;
		}
		$vars .= $indent . join(", ", @names). ": ". convertTypeName($type) . ";". $indent2;
	}
	if ($vars ne "") {
		$interface .= "var\n  $vars\n\n";
	}
	$interface .= "\n";
#print "finished variables\n";
	
	return $interface;
}

sub getDefines{
	(my $code) = @_;
	my @defines = ();
	while ($code =~ /^\#define \s+ (\w+) \s+ ($literal_no_str_markers)/xmg) {
		my $name = $1;
		my $val = $2;
		if ($val =~ /"(.*)"/) {
			$val = "'" . convertString($1) . "'";
		}
		push(@defines, "$name = $val;");
	}
	if (@defines) {
		return "const\n  " . join("\n  ", @defines) . "\n\n";
	}
	return "";
}

sub findExtraUnits
{
	(my $code, my $includes) = @_;
	foreach my $token (keys %units_to_add_by_token) {
		if ($code =~ /\b$token\b/) {
			$includes->{$units_to_add_by_token{$token}} = 1;
		}
	}
}

sub joinunits {
	(my $separator, my $arrayref) = @_;
	my $units;
	my $current_line;
	foreach my $unit (@$arrayref) {
		if ($units) {
			$unit = ', ' . $unit;
		}
		$units .= $unit;
		$current_line .= $unit;
		if (length($current_line) >= 80) {
			$units .= "\n";
			$current_line = "";
		}
	}
	return $units;
}

foreach (glob(shift @ARGV)){  
  if (/\.cpp$/) {
    /(.*)\.cpp$/i;
	$file_name = $1;
	print $file_name ."\n";
    $cpp = read_file(".\\" . $file_name . ".cpp");
    if ((! -e ".\\" . $file_name . ".h") ){ # (! -e ".\\" . $file_name . ".dfm")
      next;
    }
    $h = read_file(".\\" . $file_name . ".h");
   # $dfm = read_file(".\\" . $file_name . ".dfm");
	
	($form_class, $form_ancestor, $form_variable) = getFormName();

	$comment_number = 0;	
	@single_line_comments = ();
	@multi_line_comments = ();
	$cpp = setCommentMarkers($cpp);
	#$h = setCommentMarkers($h);
		
	%includes_to_add_to_header = ();
	findExtraUnits($h, \%includes_to_add_to_header);
	$Hincludes_ref = getIncludes($h, \%includes_to_add_to_header);
	if (%$Hincludes_ref) {
		my @array = keys %$Hincludes_ref;
		$Hincludes = "uses \n". joinunits(", ", \@array).";\n";
	}
	else {
		$Hincludes = "";
	}

	%includes_to_add = ();
	my @methods = getMethods();	
	findExtraUnits($cpp, \%includes_to_add);
	$CPPincludes_ref = getIncludes($cpp, \%includes_to_add, $Hincludes_ref);
	if (%$CPPincludes_ref) {
		my @array = keys %$CPPincludes_ref;
		$CPPincludes = "uses \n". joinunits(", ", \@array).";\n";
	}
	else {
		$CPPincludes = "";
	}
			
#	$fields{"public"} = getFields("public");

    $pas = "unit $file_name;\n\ninterface\n\n"
		   . $Hincludes ."\n"	
		   . getDefines($h)
		   ."\n\ntype\n" 
	       . getInterfaceSection()
		   #. "  $form_class = class($form_ancestor)\n"
		   #. join("\n", @methods_intf) . "\n"
		   #. "  private\n"
		   #. "  public\n"
		   #. "  end;\n\n"
		   #. "var\n"
		   #. "  $form_variable: $form_class;\n\n"
		   . "implementation\n\n"
	       . $CPPincludes . "\n\n"
		   . getDefines($cpp)
		   . "{\$R *.dfm}\n\n"
		   . join("\n$method_separator\n", @methods) . "\n$method_separator\n"
		   . "end.";

	$pas = replaceCommentMarkers($pas);
	
    # Remove empty brackets in function calls
	$pas =~ s/\(\)//g;	

#	print "$pas";
	write_file(".\\${file_name}.pas", $pas);
  }	
}
