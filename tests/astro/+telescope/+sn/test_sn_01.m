% Package Unit-Test
%
% ### Requirements for SNR Unit Test
%
%    i = Installer
%    i.install({'cats', 'PicklesStellarSpec', 'SpecGalQSO'})
%
%


function Result = unitTest()
    % telescope.sn Unit-Test
    
	io.msgStyle(LogLevel.Test, '@start', 'telescope.sn test started');
    
    snr_unitTest();
    
	io.msgStyle(LogLevel.Test, '@passed', 'telescope.sn test passed');
	Result = true;
end

%--------------------------------------------------------------------------
% # SNR Unit Test
%

function Result = snr_unitTest()

	%
	io.msgStyle(LogLevel.Test, '@start', 'telescope.sn.snr test started');
   

    % Basic form
    telescope.sn.snr()

    %
    curr_effFWHM = 12;
    
    % When Curr_Spec is numeric, it refers to 'a Black-body with temperature T'
    Curr_Spec = 2e4;
    R = 1;
    
    % Options to 'Pickles Models' combo
    Spec_all_MS_stars = AstSpec.get_pickles('M','V');
    Curr_Spec = Spec_all_MS_stars(1);

    [curr_SN] = telescope.sn.snr('FWHM', curr_effFWHM, 'TargetSpec', Curr_Spec,...
                              'FilterFamily','ULTRASAT',...
                              'ClearAper',1,'Trans',1,'Reflection',1,'QE',1)

     
    % Fails because Filter is still missing @Eran
    %[curr_SN] = telescope.sn.snr('FWHM', curr_effFWHM, 'TargetSpec', Curr_Spec,...
    %                              'FilterFamily','ULTRASAT','Filter',sprintf('R%d',R),...
    %                              'ClearAper',1,'Trans',1,'Reflection',1,'QE',1);

%     Spec_all_MS_stars = AstSpec.get_pickles([],'V');
%     Specs.Spec((numel(T_BB)+1):(numel(T_BB)+numel(Spec_all_MS_stars)))=Spec_all_MS_stars;
%     for Idwarf = 1: numel(Spec_all_MS_stars)
%         Specs.name(numel(T_BB)+Idwarf) = {Spec_all_MS_stars(Idwarf).ObjName};
%     end
%     
	io.msgStyle(LogLevel.Test, '@passed', 'telescope.sn.snr test passed');
	Result = true;
end


%--------------------------------------------------------------------------


%--------------------------------------------------------------------------


%--------------------------------------------------------------------------

