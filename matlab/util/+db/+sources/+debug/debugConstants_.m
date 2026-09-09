%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debugConstants_.m
% Author      : Chen Tishler
% Created     : 09/09/2026
% Description : Shared defaults for sources debug walkthrough scripts.
%==========================================================================

function C = debugConstants_()
%DEBUGCONSTANTS_  Return named defaults for db.sources.debug scripts.
%
% Example:
%   C = db.sources.debug.debugConstants_();

    C = struct();
    C.DefaultBaseUrl = 'http://127.0.0.1:8151';

    C.SmokeClientTimeout = 120.0;
    C.ContinuousClientTimeout = 300.0;
    C.SmokeWaitTimeout = 600.0;
    C.ContinuousWaitTimeout = 900.0;
    C.PollInterval = 2.0;

    C.OutboxPreviewMax = 5;

    C.FluxSafeMax = 319.0;
    C.CatalogSize = 1200;
    C.RaOffsetDeg = 12.0;
    C.DefaultSeed = 1042;
    C.DefaultFields = 8;

    C.VariableStarFraction = 0.02;
    C.TransientFraction = 0.005;
    C.ArtifactFraction = 0.005;
end
