% This demonstrates that the coarse mesh and the fine mesh produce the same surface profile for a given smoothness.

% Pick some input points.  There is nothing special about these and there can be as many or as few as you want.
InputPoints =[
	1, 0.5, 0.5;
	1, 2, 1;
	1, 3.5, 0.5;
	3, 0.75, 0.5;
	3, 1.5, 1;
	3, 2.25, 0.5;
	3, 3, 1;
	3, 4, 0.5];

% Change the spacing to test each axis or both axes.
% You get the same surface profile whether the spacing is 0.5 or 0.01 on either or both axes.
x = 0 : 0.2 : 4;
y = 0 : 0.2 : 4;

% Set the smoothness.  See the documentation for details about the smoothness setting.
% This demonstration shows that the surfaces are equivalent when they have the same smoothing.
Smoothness = 0.001;

% Use the updated GridFit with bicubic interpolation.  Bilinear interpolation would produce incorrect results.
z = RegularizeData3D(InputPoints(:, 1), InputPoints(:, 2), InputPoints(:, 3), x, y, 'interp', 'bicubic', 'smoothness', Smoothness);

% Plot this figure on the left.
%subplot(1, 2, 1);
subplot1 = subplot(1, 2, 1, 'Parent', figure);
view(subplot1, [-74.5, 14]);
grid(subplot1, 'on');
hold(subplot1, 'all');
% View the surface.
surf(x, y, z, 'facealpha', 0.75);
% Add the input points to see how well the surface matches them.
% The smoothness value is the only thing that controls this property of the surface.
scatter3(InputPoints(:, 1), InputPoints(:, 2), InputPoints(:, 3), 'fill');

xlabel('x');
ylabel('y');
zlabel('z');
title({['Regularized output surface with smoothness=', num2str(Smoothness), ' (coarse mesh)']; ' Original, scattered input points in blue'});
set(get(gca,'XLabel'), 'FontSize', 12)
set(get(gca,'YLabel'), 'FontSize', 12)
set(get(gca, 'Title'), 'FontSize', 12)
axis([0, 4, 0, 4, 0, 1.1]);


% Now use a really fine mesh.
x = 0 : 0.02 : 4;
y = 0 : 0.02 : 4;

% Use the updated GridFit.
z = RegularizeData3D(InputPoints(:, 1), InputPoints(:, 2), InputPoints(:, 3), x, y, 'interp', 'bicubic', 'smoothness', Smoothness);

% Plot this figure on the left.
subplot2 = subplot(1, 2, 2);
view(subplot2, [-74.5, 14]);
grid(subplot2, 'on');
hold(subplot2, 'all');
% View the surface.
surf(x, y, z, 'facealpha', 0.75);
% Add the input points to see how well the surface matches them.
% The smoothness value is the only thing that controls this property of the surface.
scatter3(InputPoints(:, 1), InputPoints(:, 2), InputPoints(:, 3), 'fill');

xlabel('x');
ylabel('y');
zlabel('z');
title({['Regularized output surface with smoothness=', num2str(Smoothness), ' (fine mesh)']; ' Original, scattered input points in blue'});
set(get(gca,'XLabel'), 'FontSize', 12)
set(get(gca,'YLabel'), 'FontSize', 12)
set(get(gca, 'Title'), 'FontSize', 12)
axis([0, 4, 0, 4, 0, 1.1]);
set(gcf, 'color', 'w')