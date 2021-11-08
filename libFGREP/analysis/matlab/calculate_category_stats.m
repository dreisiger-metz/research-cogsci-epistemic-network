function [categorymeans categorystds labels] = calculate_category_stats(means, stds)

labels = {'colours', 'heights', 'motions', 'speeds', 'textures', 'widths'};

categorymeans(1,:,:) = squeeze(mean(means(1:11,:,:)));
categorymeans(2,:,:) = squeeze(mean(means(12:24,:,:)));
categorymeans(3,:,:) = squeeze(mean(means(26:32,:,:)));
categorymeans(4,:,:) = squeeze(mean(means(33:36,:,:)));
categorymeans(5,:,:) = squeeze(mean(means(37:39,:,:)));
categorymeans(6,:,:) = squeeze(mean(means(40:52,:,:)));

categorystds(1,:,:) = squeeze(mean(stds(1:11,:,:)));
categorystds(2,:,:) = squeeze(mean(stds(12:24,:,:)));
categorystds(3,:,:) = squeeze(mean(stds(26:32,:,:)));
categorystds(4,:,:) = squeeze(mean(stds(33:36,:,:)));
categorystds(5,:,:) = squeeze(mean(stds(37:39,:,:)));
categorystds(6,:,:) = squeeze(mean(stds(40:52,:,:)));
