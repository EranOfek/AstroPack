function read2AstroImage(FileName,Args)
    %
   
    arguments
        FileName
        
        Args.ReadType = 'image';
    end

    
    Info = h5info(FileName);
    
    HeaderCell = [{Info.Groups.Attributes.Name}.', {Info.Groups.Attributes.Value}.'];
    % go over header and reformat it
    Nkey = size(HeaderCell,1);
    for Ikey=1:1:Nkey
        if iscell(HeaderCell{Ikey,2})
            HeaderCell{Ikey,2} = HeaderCell{Ikey,2}{1};
        end
    end
    TYPE
    PROJECT
    INST
    EXPTIME
    STARTTIME
    SCALE
    LIMMAG_DETECTION
    FILTER
    NAXIS
    NAXIS1
    NAXIS2
    BINX
    BINY
    TEMP_OUT
    WIND_DIR
    WIND_SPEED
    HUMID_OUT
    PRESSURE
    LIGHT
    SENSOR_TEMP
    OBJRA'            }    {'22:37:05.7'                                     }
    OBJDEC'           }    {'-08:43:23.5'                                    }
    TELRA'            }    {'22:37:06.0'                                     }
    TELDEC'           }    {'-08:43:23.4'                                    }
    OBJECT'           }    {'quadrature'                                     }
    OBSRA'            }    {'22:33:54.0'                                     }
    OBSDEC'           }    {'-08:33:29.9'                                    }
    
    FIELDROT'         }    {[                                        -59.845]}
    RA'               }    {'22:37:06.0'                                     }
    DEC'              }    {'-08:43:23.4'                                    }
    FIELD_ID'         }    {0x0 double                                       }
    HA'               }    {'-03:25:40.2'                                    }
    HA_DEG'           }    {[                                        -51.418]}
    LST'              }    {'19:11:25.8'                                     }
    ALT'              }    {[                                         26.963]}
    AZ'               }    {[                                          119.9]}
    AIRMASS'          }    {[                                         2.1977]}
    MOONAZ'           }    {[                                         183.12]}
    MOONALT'          }    {[                                         32.879]}
    MOONILL'          }    {[                                       -0.90604]}
    MOONDIST'         }    {[                                         54.601]}
    SUNAZ'            }    {[                                         42.722]}
    SUNALT'           }    {[                                        -24.174]}
    OBSLONG'          }    {[                                         34.762]}
    OBSLAT'           }    {[                                         30.597]}
    OBSEL'            }    {[                                            876]}
    

    
    
    switch lower(Args.ReadType)
        case 'image'
            Image = h5read(FileName, '/images');
            
        otherwise
            error('Not supported ReadType option');
    end

    
    
    
end