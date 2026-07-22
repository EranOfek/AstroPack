function Result = ELOPsim(Args)
    % Build a table of ULTRASAT ELOP lab-test simulation parameters and save it to a text file.
    %     The table lists the full factorial combination of the input parameter ranges
    %     (one row per combination), together with the output file names that will be
    %     used when the corresponding ultrasat.usim runs are carried out.
    %     NB: at this stage the function only builds and saves the table; it does not
    %     yet run the simulations.
    % Input : * ...,key,val,...
    %         'Filter'      - cell array of filter names. Default is {'UV','VIS'}.
    %         'Temperature' - cell array of detector temperatures [K]. Default is {200,300}.
    %         'Template'    - cell array of spatial source templates. Default is {'A','B','C','D'}.
    %         'Radius'      - cell array of radial distances of the source from the tile's
    %                         inner corner. Default is {2,3,4}.
    %         'Focus'       - cell array of focus positions. Default is {1,2,3,4,5}.
    %         'Rotation'    - cell array of rotation angles [deg]. Default is {0}.
    %         'Tile'        - a single ULTRASAT tile name, common to all the rows of the
    %                         table (not part of the combinatorial grid). Default is 'B'.
    %         'OutDir'      - output directory for the table file. Default is '.'.
    %         'OutName'     - root name used to build the per-simulation output file name
    %                         template. Default is 'USim'.
    %         'TableName'   - name of the saved parameter table text file. Default is
    %                         'ELOPsim_table.csv'.
    % Output : - a table of simulation parameters, one row per parameter combination.
    % Author : A. Krassilchtchikov (2026)
    % Example: T = ultrasat.ELOPsim();
    %          T = ultrasat.ELOPsim('Filter',{'UV'},'Focus',{1,3,5},'OutName','USimTest');
    arguments
        Args.Filter      = {'UV','VIS'};
        Args.Temperature = {200, 300};
        Args.Template    = {'A','B','C','D'};
        Args.Radius      = {2, 3, 4};
        Args.Focus       = {1, 2, 3, 4, 5};
        Args.Rotation    = {0};
        Args.Tile        = 'B';

        Args.OutDir      = '.';
        Args.OutName     = 'USim';
        Args.TableName   = 'ELOPsim_table.csv';
    end

    NumRows = numel(Args.Filter) * numel(Args.Temperature) * numel(Args.Template) * ...
              numel(Args.Radius) * numel(Args.Focus) * numel(Args.Rotation);

    N           = zeros(NumRows,1);
    Filter      = cell(NumRows,1);
    Temperature = zeros(NumRows,1);
    Template    = cell(NumRows,1);
    Radius      = zeros(NumRows,1);
    Focus       = zeros(NumRows,1);
    Rotation    = zeros(NumRows,1);
    Tile        = cell(NumRows,1);
    OutFileHI   = cell(NumRows,1);
    OutFileLO   = cell(NumRows,1);

    % build the full factorial combination of the parameter ranges (Filter varies
    % slowest, Rotation fastest), and the corresponding output file name template
    Irow = 0;
    for Ifilt = 1:numel(Args.Filter)
        for Itemp = 1:numel(Args.Temperature)
            for Itempl = 1:numel(Args.Template)
                for Irad = 1:numel(Args.Radius)
                    for Ifoc = 1:numel(Args.Focus)
                        for Irot = 1:numel(Args.Rotation)
                            Irow = Irow + 1;

                            N(Irow)           = Irow;
                            Filter{Irow}      = Args.Filter{Ifilt};
                            Temperature(Irow) = Args.Temperature{Itemp};
                            Template{Irow}    = Args.Template{Itempl};
                            Radius(Irow)      = Args.Radius{Irad};
                            Focus(Irow)       = Args.Focus{Ifoc};
                            Rotation(Irow)    = Args.Rotation{Irot};
                            Tile{Irow}        = Args.Tile;

                            BaseName = sprintf('%s_%03d_%s_%dK_Templ%s_Rad%d_F%d_Rot%d_tile%s', ...
                                Args.OutName, Irow, Filter{Irow}, Temperature(Irow), ...
                                Template{Irow}, Radius(Irow), Focus(Irow), Rotation(Irow), Tile{Irow});

                            OutFileHI{Irow} = sprintf('%s_HI.fits', BaseName);
                            OutFileLO{Irow} = sprintf('%s_LO.fits', BaseName);
                        end
                    end
                end
            end
        end
    end

    Result = table(N, Filter, Temperature, Template, Radius, Focus, Rotation, Tile, OutFileHI, OutFileLO);

    TableFullName = sprintf('%s%s%s', Args.OutDir, '/', Args.TableName);
    writetable(Result, TableFullName);

end
