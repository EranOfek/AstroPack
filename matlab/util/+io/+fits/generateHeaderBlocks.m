function HeaderStr = generateHeaderBlocks(HeaderCell)
    %
    % Example: io.fits.generateHeaderBlocks(AI.HeaderData.Data)
    
    arguments
        HeaderCell
    end

    BYTE_PER_BLOCK = 2880;
    CHAR_PER_LINE  = 80;
    KEY_LENGTH     = 8;
    
    [Nlines, Ncol] = size(HeaderCell);
    
    % number of blocks
    Nblocks = ceil(Nlines.*CHAR_PER_LINE./BYTE_PER_BLOCK);
    % allocate blocks
    HeaderStr = blanks(Nblocks.*BYTE_PER_BLOCK);
    
    for I=1:1:Nlines
%         if I==121
%             'a'
%         end
        % position of the begining of the current line
        Location = (I - 1).*CHAR_PER_LINE + 1;
        % write the keyword name
        if isempty(HeaderCell{I,1})
            HeaderCell{I,1} = ' ';
        else
            HeaderStr(Location:1:(Location+KEY_LENGTH-1)) = pad(HeaderCell{I,1}, KEY_LENGTH, 'right');
        end
        switch HeaderCell{I,1}
            case 'COMMENT'
                
            case 'HISTORY'
                
            otherwise
                
                HeaderStr((Location+KEY_LENGTH):1:(Location+KEY_LENGTH+1)) = '= ';
                if ~isempty(HeaderCell{I,2})
                    switch class(HeaderCell{I,2})
                        case 'logical'
                            % fixed value logical in char 30
                            if HeaderCell{I,2}
                                Char = 'T';
                            else
                                Char = 'F';
                            end
                            HeaderStr(Location+29) = Char;
                        case {'char'}
                            LenStr = numel(HeaderCell{I,2});
                            HeaderStr((Location+10) : (Location+11+LenStr)) = sprintf('''%s''',HeaderCell{I,2});
                        case {'int8','int16','int32','int64','uint8','uint16','uint32','uint64'}
                            % fixed value integer in chars 11 to 30
                            HeaderStr((Location+10) : (Location+29)) = sprintf('%20d', HeaderCell{I,2});
                        case {'single','double'}
                            % search for integer values saves as real
                            if abs(HeaderCell{I,2} - round(HeaderCell{I,2}))<(10.*eps)
                                % assume value is an intefer
                                % fixed value integer in chars 11 to 30
                                HeaderStr((Location+10) : (Location+29)) = sprintf('%20d', HeaderCell{I,2});
                            else
                                % fixed value read in chars 11 to 30
                                if abs(HeaderCell{I,2})>1e7 || abs(HeaderCell{I,2})<1e-4
                                    % use exponebts
                                    StrVal = sprintf('%16.13e',HeaderCell{I,2});
                                else
                                    %  
                                    StrVal = sprintf('%20.10f',HeaderCell{I,2});
                                end
                                HeaderStr((Location+10) : (Location+29)) = pad(StrVal, 20, 'left');
                            end
                    end
                end
        end
    
end