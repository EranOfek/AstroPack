function [R, R_Partial] = frt(M_in, Args)
    % Fast Radon Transform (FRT) of a 2-D matrix
    %   Calculate the FRT including partial FRT for parial lines finding
    %   using the algorithm described in Nir et al. (2018).
    %   Note that for a full FRT you need to run this function twice, the
    %   second time using the Trsnspose option.
    % Input  : - A 2-D matrix (image).
    %          * ...,key,val,...
    %            'Finder' - 
    %            'UseMex' - Use coreFRT.cpp. Default is true.
    %            'NumThreads' - Number of threads.
    %                   May speed up the code.
    %                   Default is 0.
    %            'Transpose' - Logical indicating if to work on the
    %                   trasnposed matrix. Default is false.
    %            'Pad' - pad the "active axis" to a power of 2.
    %                   If this is disabled and the matrix is not a power of 2,
    %                   the function will fail.
    %                   Default is true.
    %            'Expand' - add margins to the "passive" axis to include streaks with
    %                   starting points outside the matrix ("corner cutting streaks").
    %                   Default is false.
    % Output : - Matrix with the FRT.
    %            For example, for a 2048x2048 input, the output matrix will
    %            be 2048x4095, where the Y axis corresponds to the line
    %            intersection with the Y axis on X=0,
    %            and the X axis corresponds to the line slope between 
    %            angles of -180 to 180 deg.
    %               add info...
    %
    %          - Cell array of partial FRTs
    %            Each cell element contains the partial FRT calculated in
    %            the Radon logarithmic step. The final step contains the
    %            FRT matrix, while the one before final contains two
    %            matrices, to the two image halfs before combing, etc.
    %
    % Author : Guy Nir (Jan 2018)
    % Reference: https://ui.adsabs.harvard.edu/abs/2018AJ....156..229N/abstract
    %            Based on https://github.com/guynir42/radon
    % Example: Im = imUtil.kernel2.line([1000,1,20],[2048 2048]);
    %          [R,P] = imUtil.frt.frt(Im,'NumThreads',4);
    arguments
        M_in
        Args.UseMex logical      = true;
        Args.Finder              = [];
        Args.Transpose logical   = false;
        Args.Pad logical         = true;
        Args.Expand logical      = false;
        Args.NumThreads          = 0;
    end
        
    
    if nargout>1
        Partial = true;
    else
        Partial = false;
    end
    
    %%%%%%%%%%%%%%%%%%%%%%% CHECK INPUTS and DEFAULTS %%%%%%%%%%%%%%%%%%%%%
    
    if ndims(M_in)>2
        error('Cannot handle 3D matrices');
    end
    
    %%%%%%%%%%%%%%%%%%%%%%% PREPARING THE MATRIX %%%%%%%%%%%%%%%%%%%%%%%%%%
    M = M_in;
    
    if Args.Transpose
        M = M.';
    end

    if Args.Pad
        M = padMatrix(M); % make sure the active dim is a power of two (if not, zero pad!).
    end    

    if Args.Expand
        M = expandMatrix(M); % Increase passive dim by 3, to capture streaks coming from the side... 
    end
    
    %%%%%%%%%%%%%%%%%%%%%%% CORE ALGORITHM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    Npassive = size(M,1); % number of pixels along the passive axis (remains constant)
    
    MaxM      = floor(log2(size(M,2)));
    R_Partial = cell(1, MaxM);
    dy = 0; % the different shifts for the current M

    for m = 1:MaxM

%         disp(['m= ' num2str(m)]);

        Nactive = size(M,2); % number of pixels along the active axis (shrinks by 2 each iteration)

        M_prev = M; % make a copy of the matrix. That copy will be used to shift and add
        dy_prev = dy; % make a copy of the different shifts, relevant for M_prev 

        max_dy = 2^(m)-1; % the maximum sideways shift is equal to the height of each block (at 45 degrees)    

        dy = -max_dy:max_dy; % a vector of the possible dx values for this level of M
        M = zeros(Npassive, Nactive/2, length(dy), 'like', M_prev); % a new, empty matrix that is shorter on the active axis and bigger on the 3rd axis (dy)
                
        % entry point to mex file...
        if Args.UseMex
            if class(M) == "double"
                imUtil.frt.mex.coreFRT_double(M, M_prev, dy, Args.NumThreads);
            elseif class(M) == "single"
                imUtil.frt.mex.coreFRT_single(M, M_prev, dy, Args.NumThreads);
            end
        else

            Ndy = length(dy); % size of dy axis
            counter = 1;

            for ii = 1:ceil(Nactive/2) % number of foldings of 2 we can do to the data

                for jj = 1:Ndy % number of possible shifts for this size of slice

                    dy_minor = fix((dy(jj))/2) ; % the dy from M_prev required to fit together the current dy
                    idy_minor = dy_minor + ceil((length(dy_prev))/2); % index of dy_minor on the 3rd axis of M_prev
                    
                    gap_y = dy(jj)-dy_minor;% additional pixel gap we need to shift before we add (NOTE: gap is bigger than +-1 to bridge the difference of start to end points)

                    M1 = M_prev(:,counter,idy_minor);
                    M2 = M_prev(:,counter+1,idy_minor);

                    M(:, ii, jj) = shift_add(M1, M2, -gap_y);
                    
                end % for jj (number of shifts for each subsection)

                counter = counter+2;

            end % for ii (number of foldings in one level)

        end
        
        if ~isempty(Args.Finder)
            %Args.Finder(M, Args.FinderArgs{:});
            %finder.scan(M, transpose);
        end

        if Partial
            R_Partial{m} = M;
        end

    end % for m (logarithmic jumps)

    R = permute(M, [1,3,2]); % return just the final Radon transform
    

    % update the finder with any streaks we have found...
    if ~isempty(Args.Finder) 
        %finder.finalizeFRT(permute(M, [1,3,2]), transpose);
    end
            
end

function M_out = padMatrix(M_in, pad_value)
% pads the matrix in the row dimension so it is a power of 2. 
% usage: padMatrix(M_in, pad_value). 
% usually pad_value is equal to 0, 
% but you could imagine using nan instead. 

    if nargin<2 || isempty(pad_value)
        pad_value = 0;
    end

    S = size(M_in);
    
    twos_power = ceil(log2(size(M_in, 2)));
    S(2) = 2.^(twos_power);
    
    if pad_value==0
        M_out = zeros(S, 'like', M_in);
    elseif isnan(pad_value)
        M_out = nan(S, 'like', M_in);
    elseif isinf(pad_value)            
        M_out = inf(S, 'like', M_in);
    elseif isnumeric(pad_value)
        M_out = pad_value*ones(S, 'like', M_in);
    end
    
    M_out(:,1:size(M_in,2)) = M_in;

end

function M_out = expandMatrix(M_in, pad_value, pad_passive_factor)
% Expands a matrix: in coloum dimension, expands it by three 
% (to get streaks that start outside the frame). 
% usuage: expandMatrix(M_in, pad_value=0, pad_passive_factor=1) 
% uses zero padding unless you give another value in pad_value
% if pad_passive_factor>0 it will expand the matrix on the passive
% dimension by this factor on either side! default=1

    if nargin<2 || isempty(pad_value)
        pad_value = 0;
    end
    
    if nargin<3 || isempty(pad_passive_factor)
        pad_passive_factor = 1;
    end

    if pad_passive_factor==0
        M_out = M_in;
        return; 
    end
    
    S(1) = size(M_in,1)+floor(pad_passive_factor*2*size(M_in,2)); % add padding to allow lines at 45 degrees    
    S(2) = size(M_in,2);
    
    if pad_value==0
        M_out = zeros(S, 'like', M_in);
    elseif isnan(pad_value)
        M_out = nan(S, 'like', M_in);
    elseif isinf(pad_value)            
        M_out = inf(S, 'like', M_in);
    elseif isnumeric(pad_value)
        M_out = pad_value*ones(S, 'like', M_in);
    end

    M_out(floor(pad_passive_factor*size(M_in,2))+1:end-ceil(pad_passive_factor*size(M_in,2)), :) = M_in;
    
end

function I_out = shift_add(I1, I2, shift)

    if shift==0 % if shift is zero, do nothing! 
        I_out = I1+I2;
    elseif shift>0 && shift<size(I2,1) % positive shifts... 
        I_out = I1 + [zeros(shift,1,size(I2,3), 'like', I2); I2(1:end-shift,1,:)];
    elseif shift<0 && -shift<size(I2,1) % negative shifts...
        I_out = I1 + [I2(1-shift:end,1,:); zeros(-shift,1,size(I2,3), 'like', I2)];
    else
        I_out = I1;
    end
    
end

% This is no longer used, since we are replacing nan's with zeros in the Finder. 
% Also, this is not triggered if using the mex version (which is a lot faster, and the default). 
function I_out = shift_add_nan(I1, I2, shift)

    if shift==0 % if shift is zero, do nothing! 
        I_out = sum([I1,I2], 2, 'omitnan');
    elseif shift>0 && shift<size(I2,1) % positive shifts... 
        I_out = sum([I1, [zeros(shift,1,size(I2,3), 'like', I2); I2(1:end-shift,1,:)]], 2, 'omitnan');
    elseif shift<0 && -shift<size(I2,1) % negative shifts...
        I_out = sum([I1,[I2(1-shift:end,1,:); zeros(-shift,1,size(I2,3), 'like', I2)]], 2, 'omitnan');
    else
        I_out = I1;
        I_out(isnan(I_out)) = 0;
    end
    
end
