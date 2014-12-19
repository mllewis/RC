% creates grid of object images sorted by complexity norms
% for cogsci paper 1/15/2014
% M. Lewis



%%%%% REAL OBJECTS
cd('/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Papers/CogSci2014/figs/')

% set grid dimenensions
num_x_pics = 5;
num_y_pics = 12;

% read in norms from refComplex 9
norms = csvread('/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Papers/RC/analysis/data/complicatedNormsObjs_BYITEM.csv')
norms = sortrows(norms,1)

figure
for i=1:length(norms)
    ax(i) = subplot_tight(num_x_pics,num_y_pics,i, [0.01,0.01]);
    rgb = imread(strcat('/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Experiment_9_norm/experiment9a/refComplex processed objects2/obj_', num2str(norms(i,2)),'_p2.jpg'));
    image(rgb)
    axis off
end

linkaxes(ax,'xy')
axis(ax,'image')
set(ax, 'Visible','off')



%%%%% GEONS
cd('/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Papers/CogSci2014/figs/')

% set grid dimenensions
num_x_pics = 5;
num_y_pics = 8;

figure
counter = 1
for k=1:5
    for j = 1:8
        ax(counter) = subplot_tight(num_x_pics,num_y_pics,counter);
        rgb = imread(strcat('/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Papers/CogSci2014/figs/geons/obj', num2str(k), '-', num2str(j),'.png'));
        image(rgb)
        axis off
        counter = counter + 1;
    end
end

linkaxes(ax,'xy')
axis(ax,'image')
set(ax, 'Visible','off')

