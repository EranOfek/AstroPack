function varargout = simulateGaiaField(varargin)
    % Former name of ultrasat.simulateCatalogField (kept for old calls; use simulateCatalogField)
    % Input  : * ...,key,val,... as in ultrasat.simulateCatalogField
    % Output : as in ultrasat.simulateCatalogField
    % Author : D. Kovaleva (Oct 2026)
    % Example: [Sim, Src] = ultrasat.simulateGaiaField('Tile','B','Simulate',false);
    [varargout{1:max(nargout,1)}] = ultrasat.simulateCatalogField(varargin{:});
end
