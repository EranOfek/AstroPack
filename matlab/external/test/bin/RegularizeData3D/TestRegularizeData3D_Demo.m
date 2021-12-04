% This demonstrates the basic usage of RegularizeData3D.

% Pick some input points.  There is nothing special about these and there can be as many or as few as you want.
% The only requirement is that they cannot all be collinear with each other.
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
x = 0 : 0.16 : 4;
y = 0 : 0.16 : 4;

% Set the smoothness.  See the documentation for details about the smoothness setting.
% This demonstration shows that the surfaces are equivalent when they have the same smoothing.
Smoothness = 0.005;

% Use the updated GridFit with bicubic interpolation.  Bilinear interpolation would produce incorrect results.
z = RegularizeData3D(InputPoints(:, 1), InputPoints(:, 2), InputPoints(:, 3), x, y, 'interp', 'bicubic', 'smoothness', Smoothness);

% Plot this figure on the left.
%subplot(1, 2, 1);
subplot1 = subplot(1, 1, 1, 'Parent', figure);
set(gcf, 'color', 'white');
view(subplot1,[-74.5, 14]);
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
title({'Regularized output surface'; 'Original, scattered input points in blue'});
set(get(gca,'XLabel'),'FontSize', 12)
set(get(gca,'YLabel'),'FontSize', 12)
set(get(gca, 'Title'), 'FontSize', 14)
axis([0, 4, 0, 4, 0, 1.1]);