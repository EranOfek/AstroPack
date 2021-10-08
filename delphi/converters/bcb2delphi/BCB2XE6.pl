use File::Slurp;

sub prefixStrings {
    my $str = shift;
	$str =~ s/("([^"]|(\\"))*")/L\1/g;	
	return $str;
}

$FIBPlus_Param_calculation_callback = sub {
		my $sql = shift; 
		my $params = ""; 
		my @params = (); 
		while ($sql =~ /\s+:(\w+)/g) { 
			push(@params, $1);
		} 
		foreach my $param_name (@params){ 			
			$params = $params . "item\nName = \'$param_name\'\nParamType = ptInput\nend\n";
		} 
		return "<\n$params>\n";
	};

$FIBPlus_Macro_calculation_callback = sub {
		my $sql = shift; 
		my $macros = ""; 
		my %macros = (); 
		while ($sql =~ /@@(\w+)(?:%(.*?))?@/g) { 
			$macros{$1} = $2;
		} 
		foreach my $macro_name (keys %macros){ 			
			my $macro_val = $macros{$macro_name}; 
			if (!$macro_val) {
				$macro_val = "Null";
			} 
			$macros = $macros . "item\nName = \'$macro_name\'\nValue = '$macro_val'\nend\n";
		} 
		return "<\n$macros>\n";
	};
	
$JFProgressBarLabelCallback = sub {
	my $pbName = shift;
	my $text = $property_values{"ILabel.Caption"};
	if (!$text eq "") {
	  my $obj_name = $comp_name . "Label";
	  my $top = $property_values{"Top"} + 0;
	  my $left = $property_values{"Left"} + 0;
	  if ($top < 20) {
	    $left = $left - 50;
	  }
	  else {
	    $top = $top - 20;
	  }	  
	  return ($obj_name, "object $obj_name : TLabel\n  Caption = $text\n  Top=$top\n  Left=$left\nend\n");
	}
};

#CAREFUL : TStringField breaks unicode, so we need to use TWideStringField for UTF8 databases
#Create parameters just like macros
#TODO : implement IncludesToAdd 
@firedac_includes = ("<FireDAC.Comp.Client.hpp>", "<FireDAC.Comp.DataSet.hpp>", "<FireDAC.DApt.hpp>", "<FireDAC.DApt.Intf.hpp>", "<FireDAC.DatS.hpp>", "<FireDAC.Phys.Intf.hpp>", "<FireDAC.Stan.Async.hpp>", "<FireDAC.Stan.Error.hpp>", "<FireDAC.Stan.Intf.hpp>", "<FireDAC.Stan.Option.hpp>", "<FireDAC.Stan.Param.hpp>");

%replacements = (TpFIBQuery => {ClassName => "TFDQuery", 
                                Properties => {Database => "Connection"}, 
                                Methods => {ExecQuery => "ExecSQL", FN => "FieldByName", FBN => "FieldByName"},
                                DefaultProperties => {"MacroData" => {"SQL.Strings", $FIBPlus_Macro_calculation_callback}, "ParamData" => {"SQL.Strings", $FIBPlus_Param_calculation_callback}}, 
                                ConditionalProperties => {},
                                IncludesToAdd => \@firedac_includes}, 
                 TpFIBDatabase => {ClassName => "TFDConnection", 
                                Properties => {DBParams => "Params", DefaultTransaction => "Transaction", DefaultUpdateTransaction => "UpdateTransaction"}, 
                                Methods => {},
                                DefaultProperties => {ConnectedStoredUsage => "[auDesignTime]"},
                                ConditionalProperties => {},
                                PropertiesToRemove => ["SQLDialect", "WaitForRestoreConnect", "DBName", "Timeout", "DesignDBOptions", "SaveAliasParamsAfterConnect"],
                                IncludesToAdd => \@firedac_includes},
                 TpFIBTransaction => {ClassName => "TFDTransaction", 
                                Properties => {DefaultDatabase => "Connection", TRParams => "Params"}, 
                                Methods => {},
                                DefaultProperties => {},
                                ConditionalProperties => {},
                                PropertiesToRemove => ["TimeoutAction"],
                                IncludesToAdd => \@firedac_includes},
                 TpFIBDataSet => {ClassName => "TFDQuery", 
                                Properties => {SelectSQL => "SQL", Database => "Connection"}, 
                                Methods => {},
                                DefaultProperties => {"MacroData" => {"SelectSQL.Strings", $FIBPlus_Macro_calculation_callback}, "ParamData" => {"SelectSQL.Strings", $FIBPlus_Param_calculation_callback}}, 
                                ConditionalProperties => {"UpdateSQL.Strings" => {IFEXISTS => {"UpdateOptions.EnableUpdate" => "True", "UpdateOptions.EnableDelete" => "True", "UpdateOptions.EnableInsert" => "True"}, ELSE => {"UpdateOptions.EnableUpdate" => "False", "UpdateOptions.EnableDelete" => "False", "UpdateOptions.EnableInsert" => "False"}}},
                                PropertyTransformations => {"SelectSQL.Strings" => {'@@(\w+) (?:%.*?)? @', '"!$1"'}},
                                PropertiesToRemove => ["UpdateSQL.Strings", "RefreshSQL.Strings", "DeleteSQL.Strings", "InsertSQL.Strings", "oFetchAll", "RefreshTransactionKind", "DefaultFormats.NumericDisplayFormat", "DefaultFormats.NumericEditFormat", "poSQLINT64ToBCD", "DefaultFormats.DateTimeDisplayFormat"],
                                IncludesToAdd => \@firedac_includes},
                 TFIBIntegerField => {ClassName => "TIntegerField", Properties => {}, Methods => {}, DefaultProperties => {}, ConditionalProperties => {}, PropertyTransformations => {}},
                 TFIBStringField => {ClassName => "TWideStringField", PropertiesToRemove => ["EmptyStrToNull", "Origin"]},
                 TFIBBCDField => {ClassName => "TBCDField", PropertiesToRemove => ["RoundByScale"]},
                 TFIBBlobField => {ClassName => "TBlobField"},
                 TFIBMemoField => {ClassName => "TMemoField"},
                 TFIBFloatField => {ClassName => "TFloatField", Properties => {}, Methods => {}, DefaultProperties => {}, ConditionalProperties => {}, PropertyTransformations => {}},
                 TFIBDateTimeField => {ClassName => "TSQLTimeStampField"},
				 TFIBSmallIntField => {ClassName => "TSmallIntField"},
				 TSIBfibEventAlerter => {ClassName => "TFDEventAlerter", Properties => {Events => "Names", Database => "Connection", OnEventAlert => "OnAlert"}},
                 TQImport2XLS => {ClassName => "TQImport3XLS"},
                 TJvBalloonHint => {ClassName => "TBalloonHint"},
                 TJfProgressBar => {ClassName => "TProgressBar", ExtraComponents => [$JFProgressBarLabelCallback], DefaultProperties => {Style => "pbstMarquee"}, PropertiesToRemove => ["TypeBar", "ILabel.Text.SetFont.Name", "SetBackGround.Name", "SetBGProgress.Name", "MarginLeftRight", "MarginTopBottom", "WidthBlock", "ILabel.Caption"]},
                 TJvSaveDialog => {ClassName => "TSaveDialog", PropertiesToRemove => ["Width", "Height"]},
				 TJfCApplication => {ClassName => ""},
				 TJfCForm => {ClassName => ""},
				 TWPPDFPrinter => {ClassName => ""},
				 TDBJPEGImage => {ClassName => "TJvDBImage", IncludesToAdd => ["<JvDBImage.hpp>"]},
				 TJPEGFileCompressor => {ClassName => ""},
                 TJPEGFileDecompressor => {ClassName => ""},
                 TKADaoDatabase => {ClassName => ""},
                 TKADaoTable => {ClassName => ""},
				 TMemoryTable => {ClassName => "TRxMemoryData"},
                 TTntDBEdit => {ClassName => "TDBEdit"},
				 TTntDBGrid => {ClassName => "TDBGrid"},
				 TTntDBMemo => {ClassName => "TDBMemo"},
				 TTntDBRadioGroup => {ClassName => "TRadioGroup"},
				 TTntDBText => {ClassName => "TDBText"},
				 TTntLabel => {ClassName => "TLabel"},
				 TTntMemo => {ClassName => "TMemo"},
				 TTntPanel => {ClassName => "TPanel"},
				 TTntQRDBText => {ClassName => "TQRDBText"},
				 TTntRichEdit => {ClassName => "TRichEdit"},
				 TTntStringField => {ClassName => "TStringField"},
				 TIdFTP => {ClassName => "TIdFTP"}, PropertiesToRemove => ["MaxLineAction"],
                );
				
@includes_to_remove = ("FIBDatabase", "pFIBDatabase", "pFIBDataSet", "pFIBQuery", "FIBQuery", "FIBDataSet", "JfPanel", "JfProgressBar", "JfMain", "JvBalloonHint", "dxPScxGrid6Lnk", "WPPDFR1", "WPPDFR2", "SIBEABase", "SIBFIBEA", 
					"TntDBGrids", "TntDB", "TntDbCtrls", "TntExtCtrls", "TntStdCtrls", "mwadbjpg");
%tokens_to_change = (TGridCoordEh => "TGridCoord", TGridDrawStateEh => "Gridseh::TGridDrawState", "Animate.hpp" => "RxAnimate.hpp", "AnsiString" => "String");
%includes_to_rename = ("ToolEdit" => "RxToolEdit");

foreach (glob(shift @ARGV)){  
  if (/\.cpp$/) {
    /(.*)\.cpp$/i;
	$file_name = $1;
    $cpp = read_file(".\\" . $file_name . ".cpp");
    if ((! -e ".\\" . $file_name . ".h") or (! -e ".\\" . $file_name . ".dfm")){
      next;
    }
    $h = read_file(".\\" . $file_name . ".h");
    $dfm = read_file(".\\" . $file_name . ".dfm");
	
	# Remove old includes and pragma links from .cpp and .h
	foreach my $inc (@includes_to_remove) {	    
		$cpp =~ s/^\#pragma link ["<]${inc}[">]\h*\n//img;
		$cpp =~ s/^\#include\s+(<|")${inc}\.hpp("|>)\h*\n//img;
		$h =~ s/^\#include\s+(<|")${inc}\.hpp("|>)\h*\n//img;
	}

	# Rename includes
	foreach my $inc (keys %includes_to_rename) {	    
		$cpp =~ s/(^\#pragma \s+ link \s+ ["<]) ${inc} ([">]\h*\n)/
					$1 . $includes_to_rename{$inc} . $2
				/imgxe;
		$cpp =~ s/(^\#include \s+ ["<]) ${inc} (\.hpp [">] \h*\n)/
					$1 . $includes_to_rename{$inc} . $2
				/imgxe;
		$h   =~ s/(^\#include \s+ ["<]) ${inc} (\.hpp [">] \h*\n)/
					$1 . $includes_to_rename{$inc} . $2
				/imgxe;
	}

	# Get list of existing includes
	$getIncludes = qr/^\#include \s+ (?: <|") (\w+) \.h (?: pp) (?: "|>) \s* $/xm;
	%HEADERincludes = map{$_ => 1} ($h =~ /$getIncludes/g);
	#%CPPincludes = map{$_ => 1} ($cpp =~ /$getIncludes/g);

	# List of includes to add
	# Filled automatically while reading DFM, according to the IncludesToAdd property
	%includes_to_add_to_header = ();
	#%includes_to_add_to_cpp = ();
	
	# Change tokens from old format to new
	foreach my $token (keys %tokens_to_change) {
            $cpp =~ s/\b${token}\b/$tokens_to_change{$token}/img;
            $h =~ s/\b${token}\b/$tokens_to_change{$token}/img;            
	}
	
	# Remove weird quote mark at end of define in the .h
	$h =~ s/(^#((ifndef)|(define))\s+${file_name}H)"/$1/img;
	
	# Change syntax of Application->MessageBox calls
	#$cpp =~ s/(\bApplication\s*->\s*MessageBox\s*) 
	#                   (\( ([^"\(\)]|("([^"]|(\\"))*")|(?2))* \))
	#		 /$1 . prefixStrings($2) /exg; 
	
	
    %comps = ($dfm =~ m/object\s+(\w+)\s*:\s*(\w+)/ig);
    foreach $comp_name (keys %comps) {
      $comp_class = $comps{$comp_name};
      if (exists $replacements{$comp_class}) {
        $new_class = $replacements{$comp_class}->{'ClassName'};
		$property_replacements = $replacements{$comp_class}->{'Properties'};
		$method_replacements = $replacements{$comp_class}->{'Methods'};
		$default_properties = $replacements{$comp_class}->{'DefaultProperties'};
		$conditional_properties = $replacements{$comp_class}->{'ConditionalProperties'};
		$property_transformations = $replacements{$comp_class}->{'PropertyTransformations'};
		$properties_to_remove = $replacements{$comp_class}->{'PropertiesToRemove'};
		$extra_components = $replacements{$comp_class}->{'ExtraComponents'};
		$extra_includes = $replacements{$comp_class}->{'IncludesToAdd'};
		
		foreach my $extra_inc (@$extra_includes) {
			$includes_to_add_to_header{$extra_inc} = 1;
		}
		
		#Replace object's class name in .h 
		if ($new_class eq "") {
		  # If no replacement class is specified, component should be deleted
		  $h =~ s/\b${comp_class}\s*\*\s*${comp_name}\b\s*;\s*//i;
		  $dfm =~ s/^\s*object\s+${comp_name}\s*:\s*${comp_class}\s*$
				  # Get list of properties and values
				  ([\s\S]*?)
				  ((?:^\s*object\s+ \w+\s*:\s*\w+\s*$
				     (?:[\s\S]*?) 
				     ^\s*end\s*
				  )*)
				  ^\s*end\s*$ (?! ^\s* item \s*$ )
				//mix;
		}
		else {
          $h =~ s/\b${comp_class}\b/$new_class/g;
          $cpp =~ s/\b${comp_class}\b/$new_class/g;
	    

		  #Get list of property values
          %property_values = ();		
		  $dfm =~ /^\s*object\s+${comp_name}\s*:\s*${comp_class}\s*$
				  # Get list of properties and values
				  ([\s\S]*?)
				  ((?:^\s*object\s+ \w+\s*:\s*\w+\s*$
				     (?:[\s\S]*?) 
				     ^\s*end\s*
				  )*)
				  ^\s*end\s*$ (?! ^\s* item \s*$ )/mgix;
  		  $properties = $1;
		  $subproperties = $2;
		
		  #print $comp_name . "\n";		
                while ($properties =~ /^\s*((?:\w|\.)+?)\s*=\s*(   # Property name : captured as $1
				  
						# Property values : captured as $2
						# Each data type is handled separately below
						# String values
						(?:
							\(?\s*				  
							(?:'(?:[^']|''|'\#\d+')*' (?:\s*\#\d+)* \s*\+?\s*)+							
							\s*\)?
						)
					
						#Binary (Hex) values
						|(?: \{\s*(?:(?:[A-F]|[0-9])+\s*)*\})
					
						#Numeric or identifier values
						|(?: [\w.-]+\s*)
					
						#Sets 
						|(?: \[(?:[\w.-]+,?\s*)*\])
						
						#Collections
						|(?: <\s*
						       ^\s*item\s*$
						       (?R)+
						       ^\s*end\b
						     > 						       
                         )
					)\s*$/migx) 
		{			  
		  #print $1 . "\n";
		  $property_values{$1} = $2;
                }		
		
		my $extra_components_text = "";
		foreach my $comp (@$extra_components){
			(my $extra_comp_name, my $extra_comp_text) = $comp->();
			$extra_components_text = $extra_components_text . "\n" . $extra_comp_text;
		}
		
		foreach $prop (keys %$conditional_properties) {
                    if (exists $property_values{$prop}) {
                      $props_to_insert = $conditional_properties->{$prop}->{"IFEXISTS"};
                    }
                    else {
                      $props_to_insert = $conditional_properties->{$prop}->{"ELSE"};
                    }
                    
                    foreach $prop_name (keys %$props_to_insert) {
                      $property_values{$prop_name} = $props_to_insert->{$prop_name};    
                    }
		}
		
		foreach $prop (keys %$default_properties) {
		  #print $prop; #. " " . $default_properties->{$prop}."\n";
		    my $prop_val = $default_properties->{$prop};
		    if (ref($prop_val) eq "HASH") {
		      ($source_prop_name) = keys %$prop_val;
		      $source_prop_val = $property_values{$source_prop_name};
		      $property_values{$prop} = $prop_val->{$source_prop_name}($source_prop_val);
		    }
		    else {
                      $property_values{$prop} = $default_properties->{$prop};
                    }
		}
		
		foreach $prop (keys %$property_transformations) {
		    $regex_parts = $property_transformations->{$prop};
		    ($search) = keys %$regex_parts;
		    $replace = $regex_parts->{$search};
		    if (exists $property_values{$prop}){
                      $prop_value = $property_values{$prop};
                      $prop_value =~ s/$search/$replace/ixgmee;
                      $property_values{$prop} = $prop_value;
                    }
		}
		
		foreach $prop (@$properties_to_remove) {
		    #print $prop;
                    delete $property_values{$prop};		    
		}
		
		#Apply property replacements to DFM property values
		foreach $prop (keys %property_values) {          
		  $prop_name = $prop;
		  if (!exists $property_replacements->{$prop_name}) {
			$prop_name =~ s/(\w+)(\.\w+)*/$1/imx;
			$prop_subname = $2;	
		  }
		  if (exists $property_replacements->{$prop_name}) {
		    #print "Replacing property $prop_name by $property_replacements->{$prop_name}\n";
		    $property_values{$property_replacements->{$prop_name} . $prop_subname} = $property_values{$prop};
                    delete $property_values{$prop};		    
		  }
		}

		#next;
		
		@props = ();
		foreach $prop (keys %property_values) {
		  push(@props, "$prop = $property_values{$prop}");
		}	
		$props = join("\n", @props);
		$props = $props . "\n" . $subproperties;
		
		#Replace class name and property values in .dfm
		$dfm =~ s/^\s*object\s+${comp_name}\s*:\s*${comp_class}\s*$
				  # Get list of properties and values
				  ([\s\S]*?)
				  ((?:^\s*object\s+ \w+\s*:\s*\w+\s*$
				     (?:[\s\S]*?) 
				     ^\s*end\s*
				  )*)
				  ^\s*end\s*$ (?! ^\s* item \s*$ )
				/object $comp_name : $new_class\n$props\nend\n$extra_components_text/mix;
		
		#Apply property name replacements to cpp
		foreach $prop (keys %$property_replacements) {
          	  $cpp =~ s/(?<!->)                     # Make sure object name isn't preceded by -> nor .
		           \s*\b$comp_name\s*->\s*\K$prop\b    # Get property name, matching whole words only. \K removes the object name from the match
				   /$property_replacements->{$prop}/xg;
		}
		#Apply method name replacements to cpp
		foreach $method_name (keys %$method_replacements) { 
          	  $cpp =~ s/(?<!->)                     # Make sure object name isn't preceded by -> nor .
		           \s*\b$comp_name\s*->\s*\K$method_name\b    # Get property name, matching whole words only. \K removes the object name from the match
				   /$method_replacements->{$method_name}/xg;
		}
		}		
	  }
    }

	foreach my $incToAdd (keys %includes_to_add_to_header) 
	{	    
		if (not $HEADERincludes{$incToAdd}) {
			$h =~ s/([\s\S]+ ^ \#include .*? \h*\n)/$1#include $incToAdd\n/xm;
		}
	}
	
	#foreach my $incToAdd (keys %includes_to_add_to_cpp) 
	#{	    
	#	if (not $CPPincludes{$incToAdd}) {
	#		$cpp =~ s/([\s\S]+ ^ \#include .*? \h*\n)/$1#include <$incToAdd>\n/xm;
	#	}
	#}

	#Write files back to disk
	write_file($file_name . ".cpp", $cpp);
	write_file($file_name . ".dfm", $dfm);
	write_file($file_name . ".h", $h);
  }
}
