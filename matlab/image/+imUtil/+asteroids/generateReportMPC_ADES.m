function [Result, Sent] = generateReportMPC_ADES(Table, FileName, Args)
    % Generte MPC report in new XML ADES format
    % Input  : - Table
    %          - File name in which to write report.
    %          * ...,key,val,... 
    %            See code for options.
    % Output : - A structure with:
    %            .docNode - XML doc node.
    %          - A flag indicating if the report was sent.
    % Bug fix: 2025-Jul-14 logSNR twice 
    % Documentation: https://minorplanetcenter.net/mpcops/documentation/valid-ades-values/#astCat
    %           https://minorplanetcenter.net/iau/info/ADES.html
    %           https://github.com/IAU-ADES/ADES-Master/blob/master/ades_master.pdf
    %           https://minorplanetcenter.net/submit_xml?method=post
    % Author : Eran Ofek (2025 Mar) 
    % Example: imUtil.asteroids.generateReportMPC_ADES

    arguments
        Table
        FileName                     = 'test.xml';
        Args.ObsCode                 = "M01";
        Args.ObsName                 = "LAST";
        Args.Submitter               = ["E. Ofek"]; %["D. Polishook"]; %, "E. Ofek"];
        Args.Observer                = ["L. Auto"];
        Args.Measurer                = ["L. Pipeline", "E. Ofek"];
        Args.TelescopeDesign         = "Rowe-Ackerman Schmidt";
        Args.TelescopeAper           = "0.28";
        Args.Detector                = "CCD";
        Args.Comment                 = ["LAST Node 01, Mount 05, Camera 01", "Each measurement is based on a linear fit to 20x20s exposures"];
        
        Args.ColPermID               = 'Number';
        Args.ColProvID               = 'Designation';
        Args.TrkSubPrefix            = 'L';   % prefix to add to trkSub if numeric
        Args.ColTrkSub               = 'AstIndex';                  % column for: Observer-assigned tracklet identifier,
                                                         % unique within a submission batch. Not
   
                                                         % altered by the
                                                         % MPC. 8 char
        Args.ColProg                 = 'ProgramMPC';   % 2 chars assigned by MPC for observatory program
        Args.ColJD                   = 'JD';
        Args.ColTimeRMS              = 0.5;
        Args.ColRA                   = 'RA';   % deg
        Args.ColDec                  = 'Dec';  % deg
        Args.ColErrRA                = 'ErrRA';   % arcsec
        Args.ColErrDec               = 'ErrDec';  % arcsec
        Args.ColCorrRADec            = 'CorrRADec';
        
        Args.AstCat                  = 'Gaia3';
        Args.ColMag                  = 'MAG_PSF';
        Args.ColErrMag               = 'MAGERR_PSF';   % or numeric value
        Args.Band                    = 'c';
        Args.ColSN                   = 'SN';
        Args.ColSeeing               = 'FWHM';
        Args.PhotCat                 = 'Gaia3';
        Args.ColExpTime              = 'ExpTime';  % or a number

        Args.SendReport              = false;
        Args.EMail                   = '';
        Args.AckMessage              = [];
    end
    
    
    % Create an XML Document
    docNode = com.mathworks.xml.XMLUtils.createDocument('ades');
    
    % Set the ades root node attribute
    adesElem = docNode.getDocumentElement;
    adesElem.setAttribute('version', '2017');
    
    % Create obsBlock
    obsBlock = docNode.createElement('obsBlock');
    adesElem.appendChild(obsBlock);
    
    % Create obsContext
    obsContext = docNode.createElement('obsContext');
    obsBlock.appendChild(obsContext);
    
    % Add observatory details
    observatory = docNode.createElement('observatory');
    obsContext.appendChild(observatory);
    
    addTextElement(docNode, observatory, 'mpcCode', Args.ObsCode);
    addTextElement(docNode, observatory, 'name', Args.ObsName);
    
    % Add submitter details
    submitter = docNode.createElement('submitter');
    obsContext.appendChild(submitter);
    
    for I=1:1:numel(Args.Submitter)
        addTextElement(docNode, submitter, 'name', Args.Submitter{I});
    end
    
    % Add observers
    observers = docNode.createElement('observers');
    obsContext.appendChild(observers);
    
    for I=1:1:numel(Args.Observer)
        addTextElement(docNode, observers, 'name', Args.Observer{I});
    end
    
    % Add measurers
    measurers = docNode.createElement('measurers');
    obsContext.appendChild(measurers);
    
    for I=1:1:numel(Args.Measurer)
        addTextElement(docNode, measurers, 'name', Args.Measurer{I});
    end
    
    % Add telescope details
    telescope = docNode.createElement('telescope');
    obsContext.appendChild(telescope);
    
    addTextElement(docNode, telescope, 'design', Args.TelescopeDesign);
    addTextElement(docNode, telescope, 'aperture', Args.TelescopeAper);
    addTextElement(docNode, telescope, 'detector', Args.Detector);
    
    % Add funding source
    %addTextElement(docNode, obsContext, 'fundingSource', 'Name of Funding Agency');
    
    % Add comments
    comment = docNode.createElement('comment');
    obsContext.appendChild(comment);
    
    for I=1:1:numel(Args.Comment)
        addTextElement(docNode, comment, 'line', Args.Comment{I});
    end
    
    % Create obsData
    obsData = docNode.createElement('obsData');
    obsBlock.appendChild(obsData);
    
    Nobs = size(Table,1);
    for Iobs=1:1:Nobs
        % Create optical
        optical = docNode.createElement('optical');
        obsData.appendChild(optical);
    
        % Add optical observation details
        if tools.table.isColumn(Table, Args.ColPermID)
            if ~isnumeric(Table.(Args.ColPermID)(Iobs))
                PermID = sprintf('%s',Table.(Args.ColPermID)(Iobs));
            else
                PermID = sprintf('%d',Table.(Args.ColPermID)(Iobs));
            end
            addTextElement(docNode, optical, 'permID', PermID);
        end
        if tools.table.isColumn(Table, Args.ColProvID)
            if ~isempty(Table.(Args.ColProvID)(Iobs))
                addTextElement(docNode, optical, 'provID', Table.(Args.ColProvID)(Iobs));
            end
        end
        
        if tools.table.isColumn(Table, Args.ColTrkSub)
            if isnumeric(Table.(Args.ColTrkSub)(Iobs))
                TrkSub = sprintf('%s%d',Args.TrkSubPrefix, Table.(Args.ColTrkSub)(Iobs));
            else
                TrkSub = Table.(Args.ColTrkSub){Iobs};
            end
            addTextElement(docNode, optical, 'trkSub', TrkSub);
        end
        
        
        addTextElement(docNode, optical, 'mode', Args.Detector);
        
        addTextElement(docNode, optical, 'stn', Args.ObsCode);
        
        if tools.table.isColumn(Table, Args.ColProg)
            Prog = sprintf('%2d',Table.(Args.ColProg)(Iobs));
            addTextElement(docNode, optical, 'prog', Prog);
        end
        
        if isnumeric(Table.(Args.ColJD))
            % JD is provided - convert to date
            Date = convert.time(Table.(Args.ColJD)(Iobs),'JD','StrDate');
            Date = [Date{1}, 'Z'];
        
            addTextElement(docNode, optical, 'obsTime', Date);
        else
            error('Requires JD');
        end
        
        if ~isempty(Args.ColTimeRMS)
            if isnumeric(Args.ColTimeRMS)
                addTextElement(docNode, optical, 'rmsTime', sprintf('%8.5f',Args.ColTimeRMS));
            else
                if tools.table.isColumn(Table, Args.ColTimeRMS)
                    addTextElement(docNode, optical, 'rmsTime', sprintf('%8.5f',Table.(Args.ColTimeRMS)(Iobs)));
                end
            end
        end
        
        
        addTextElement(docNode, optical, 'ra',  sprintf('%11.7f',Table.(Args.ColRA)(Iobs)));
        addTextElement(docNode, optical, 'dec', sprintf('%11.7f',Table.(Args.ColDec)(Iobs)));
        
        if tools.table.isColumn(Table, Args.ColErrRA)
            addTextElement(docNode, optical, 'rmsRA', sprintf('%5.3f',Table.(Args.ColErrRA)(Iobs)));
        end
        if tools.table.isColumn(Table, Args.ColErrDec)
            addTextElement(docNode, optical, 'rmsDec', sprintf('%5.3f',Table.(Args.ColErrDec)(Iobs)));
        end
        
        if tools.table.isColumn(Table, Args.ColCorrRADec)
            addTextElement(docNode, optical, 'rmsCorr', sprintf('%6.3f',Table.(Args.ColCorrRADec)(Iobs)));
        end
        
        
        
        addTextElement(docNode, optical, 'astCat', Args.AstCat);
        
        addTextElement(docNode, optical, 'mag', Table.(Args.ColMag)(Iobs));
        if isnumeric(Args.ColErrMag)
            addTextElement(docNode, optical, 'rmsMag', sprintf('%4.2f',Args.ColErrMag));
        else
            addTextElement(docNode, optical, 'rmsMag', sprintf('%4.2f',Table.(Args.ColErrMag)(Iobs)));
        end
        addTextElement(docNode, optical, 'band', Args.Band);
        addTextElement(docNode, optical, 'photCat', Args.PhotCat);
        %addTextElement(docNode, optical, 'photAp', '13.3');
        if tools.table.isColumn(Table, Args.ColSN)
            addTextElement(docNode, optical, 'logSNR', sprintf('%4.2f',log10(Table.(Args.ColSN)(Iobs))));
        end
        if tools.table.isColumn(Table, Args.ColSeeing)
            addTextElement(docNode, optical, 'seeing', sprintf('%3.1f',(Table.(Args.ColSeeing)(Iobs))));
        end
        
        %if tools.table.isColumn(Table, Args.ColSeeing)
        %    addTextElement(docNode, optical, 'logSNR', log10(Table.(Args.ColSN)(Iobs)));
        %end
        
        if ~isempty(Args.ColExpTime)
            if isnumeric(Args.ColExpTime)
                ExpTime = sprintf('%6d',Args.ColExpTime);
            else
                ExpTime = sprintf('%6d',Table.(Args.ColExpTime)(Iobs));
            end
            addTextElement(docNode, optical, 'exp', ExpTime);
        end
        %addTextElement(docNode, optical, 'notes', 'klmnp');
        %addTextElement(docNode, optical, 'remarks', 'High winds affected tracking');
    end
    
    % Write to XML file
    xmlwrite(FileName, docNode);
    
    disp(['XML file created: ', FileName]);
    Result.docNode = docNode;

    %function addTextElement(doc, parent, tag, text)
    % Helper function to add an element with text content
    %elem = doc.createElement(tag);
    %elem.appendChild(doc.createTextNode(text));
    %parent.appendChild(elem);
    %end

    Sent = false;
    if Args.SendReport
        if isempty(Args.AckMessage)
            Args.AckMessage = FileName;
        end
    
        SendReportStr = sprintf('curl -L https://minorplanetcenter.net/submit_xml -F "ack=%s" -F "ac2=%s" -F "obj_type=Unclassified" -F "source=%s"',Args.AckMessage, Args.EMail, FileName);

        [Report.Status, Report.CmdOut] = system(SendReportStr);
        Sent = true;
        
    end
    
end

function addTextElement(doc, parent, tag, text)
    % Helper function to add an element with text content
    elem = doc.createElement(tag);
    elem.appendChild(doc.createTextNode(string(text)));
    parent.appendChild(elem);
end

