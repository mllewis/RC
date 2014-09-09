% creates grid of object images sorted by complexity norms
% 1/15/2014
% M. Lewis



%%%%% REAL OBJECTS %%%%%

% set grid dimenensions
num_x_pics = 5;
num_y_pics = 12;

% read in norms from refComplex 9
norms = csvread('/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Papers/RC/analysis/data/complicatedNormsObjs_BYITEM-m.csv')

figure
for i=1:length(norms)
    ax(i) = subplot_tight(num_x_pics,num_y_pics,i, [0.01,0.01]);
    rgb = imread(strcat('/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Papers/RC/stimuli/real novel objects/obj_', num2str(norms(i,1)), '.jpg'));
    image(rgb)
    axis off
end

linkaxes(ax,'xy')
axis(ax,'image')
set(ax, 'Visible','off')



%%%%% GEONS %%%%%

% set grid dimenensions
num_x_pics = 5;
num_y_pics = 8;

figure
counter = 1
for k=1:5
    for j = 1:8
        ax(counter) = subplot_tight(num_x_pics,num_y_pics,counter);
        rgb = imread(strcat('/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Papers/RC/stimuli/geons/obj', num2str(k), '-', num2str(j),'.png'));
        image(rgb)
        axis off
        counter = counter + 1;
    end
end

linkaxes(ax,'xy')
axis(ax,'image')
set(ax, 'Visible','off')