function HeaderStr = generateHeaderBlocks(HeaderCell, DataClass, DataSize)
    % Given a 3 column cell array of an header, create a string containing
    %       the entire FITS header blocks.
    % Input  : - A 3 columns cell array (3rd column ignored).
    % Output : - A string containing N*2880 bytes of the entire FITS header.
    % AUthor : Eran Ofek (Jan 2022)
    % Example: HeaderStr = io.fits.generateHeaderBlocks(AI.HeaderData.Data)
    
    arguments
        HeaderCell
        DataClass = [];
        DataSize  = [];
    end

    BYTE_PER_BLOCK = 2880;
    CHAR_PER_LINE  = 80;
    KEY_LENGTH     = 8;
    
    % clean header
    
    Flag = all(cellfun(@isempty, HeaderCell(:,1:2)),2);
    HeaderCell = HeaderCell(~Flag,:);
    
    [Nlines, Ncol] = size(HeaderCell);
 
    % number of blocks: +1 because need to add the END keyword
    Nblocks = ceil((Nlines+1).*CHAR_PER_LINE./BYTE_PER_BLOCK);
    % allocate blocks
    %HeaderStr = blanks(Nblocks.*BYTE_PER_BLOCK);
    HeaderStr = repmat(' ',[1 Nblocks.*BYTE_PER_BLOCK]);
    
    
    I = 0;
    for Iline=1:1:Nlines

        % position of the begining of the current line        
        I = I + 1;
        Location = (I - 1).*CHAR_PER_LINE + 1;
        % write the keyword name
        if isempty(HeaderCell{Iline,1})
            HeaderCell{Iline,1} = ' ';
        else
            %HeaderStr(Location:1:(Location+KEY_LENGTH-1)) = pad(HeaderCell{Iline,1}, KEY_LENGTH, 'right');
            HeaderStr(Location:1:(Location+numel(HeaderCell{Iline,1})-1)) = HeaderCell{Iline,1};
        end
        switch HeaderCell{Iline,1}
            case {'COMMENT','HISTORY'}
                HeaderStr(Location:(Location+6)) = HeaderCell{Iline,1};
                StrLength = numel(HeaderCell{Iline,2});
                HeaderStr((Location+8):(Location+9+StrLength)) = sprintf('''%s''',HeaderCell{Iline,2});

            otherwise

                HeaderStr((Location+KEY_LENGTH):1:(Location+KEY_LENGTH+1)) = '= ';
                if ~isempty(HeaderCell{Iline,2})
                    switch class(HeaderCell{Iline,2})
                        case 'logical'
                            % fixed value logical in char 30
                            if HeaderCell{Iline,2}
                                Char = 'T';
                            else
                                Char = 'F';
                            end
                            HeaderStr(Location+29) = Char;
                        case {'char'}
                            LenStr = numel(HeaderCell{Iline,2});
                            HeaderStr((Location+10) : (Location+11+LenStr)) = sprintf('''%s''',HeaderCell{Iline,2});
                        case {'int8','int16','int32','int64','uint8','uint16','uint32','uint64'}
                            % fixed value integer in chars 11 to 30
                            HeaderStr((Location+10) : (Location+29)) = sprintf('%20d', HeaderCell{Iline,2});
                        case {'single','double'}
                            % search for integer values saves as real
                            if abs(HeaderCell{Iline,2} - round(HeaderCell{Iline,2}))<(10.*eps)
                                % assume value is an intefer
                                % fixed value integer in chars 11 to 30
                                HeaderStr((Location+10) : (Location+29)) = sprintf('%20d', HeaderCell{Iline,2});
                            else
                                % fixed value read in chars 11 to 30
                                if abs(HeaderCell{Iline,2})>1e7 || abs(HeaderCell{Iline,2})<1e-4
                                    % use exponebts
                                    StrVal = sprintf('%16.13e',HeaderCell{Iline,2});
                                else
                                    %  
                                    StrVal = sprintf('%20.10f',HeaderCell{Iline,2});
                                end
                                HeaderStr((Location+10) : (Location+29)) = sprintf('%20s',StrVal);
                                %pad(StrVal, 20, 'left');                                    
                            end
                    end
                end
        end
    end
    % add the END keyword
    if numel(HeaderCell{Nlines,1})==3 && strcmp(HeaderCell{Nlines,1}, 'END')
        % no need to add END keyword
    else
        I        = I + 1;
        Location = (I - 1).*CHAR_PER_LINE + 1;
        HeaderStr(Location:(Location+2)) = 'END';
    end
end