function PC_array = broadcastJointFitResult(PC_array, JointCF)
    % Broadcast a fitted joint CompositeFun to every PhotCalibTrans in the
    % array by assigning the CompositeFun handle into each PC's TransModel
    % slot. After this call, every PC(i).TransModel is the SAME CompositeFun
    % handle — modifying one modifies all 24. That handle-sharing is
    % deliberate (see [[photcalibtrans_joint_visit_plan]] Q2=(b)): the
    % joint fit produced one set of params (1 Norm + 10 field-frame Tran2D
    % coefs + shared atm) and we want every crop to see the same physical
    % model.
    %
    % Refactored Apr 2026: the previous signature took (FieldTran2DObj,
    % Norm, Tran2DCoefs) and unpacked them per-PC. Now the joint fit is
    % done through a real CompositeFun.fitMultiStage, so we just hand the
    % fitted CompositeFun across.
    % Input  : - PC_array - 1xN PhotCalibTrans array to receive results.
    %          - JointCF  - fitted CompositeFun handle from
    %                       fitJointVisit's fitMultiStage call.
    % Output : - PC_array - mutated array. Each PC(i).TransModel points to
    %                       JointCF (handle assignment, not deep copy).
    % Author : D. Kovaleva (April 2026)
    % Example: PC = PhotCalibTrans.broadcastJointFitResult(PC, JointCF);

    arguments
        PC_array
        JointCF
    end

    Ncrops = numel(PC_array);
    for I = 1:Ncrops
        PC_array(I).TransModel = JointCF;
    end
end
