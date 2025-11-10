clear all;clc;
addpath(genpath('/bcbl/home/public/LEPA/SPMResults/spm12'))
basedir = '/bcbl/home/home_g-m/llecca/soft/paper-DevTrajThalTract/4Leandro';
container='anatrois';
version='4.5.2-7.3.2';
analysis='analysis-01';

projPath = fullfile(basedir,'nifti','derivatives',...
    strcat(container,'_',version),analysis);

subSesList    = fullfile(projPath,'subSesList.txt');
opts          = detectImportOptions(subSesList);
opts = setvartype(opts,{'sub','ses','RUN','anat','dwi','func'},...
                       {'categorical',...
                       'categorical',...
                       'logical',...
                       'logical',...
                       'logical',...
                       'logical'});
ssl           = readtable(subSesList,opts); 
subs = unique(ssl.sub);
ssl = ssl(ssl.dwi==true,:);
rows = unique(ssl(:,{'sub','ses'}));

% % Define ROIs list to merge and their merged name

ROIformat = '.nii.gz';

ROIlist = {{'Left-V1','Left-V2'},...
           {'Right-V1','Right-V2'},...
           {'L_Area_1','L_Area_2','L_Area_3a','L_Primary_Sensory_Cortex'},...
           {'R_Area_1','R_Area_2','R_Area_3a','R_Primary_Sensory_Cortex'},...
           {'ctx_lh_G_cingul-Post-dorsal','ctx_lh_G_cingul-Post-ventral'},...
           {'Left-MDl','Left-MDm'},...
           {'Right-MDl','Right-MDm'},...
           {'Left-Basal-nucleus','Left-Lateral-nucleus'},...
           {'Right-Basal-nucleus','Right-Lateral-nucleus'}};
for rl = 1:length(ROIlist)
    ROIlist{rl} = append(ROIlist{rl},ROIformat);
end

mergedROInames = {'Left-V1_AND_Left-V2',...
                  'Right-V1_AND_Right-V2',...
                  'L_Area_1_AND_L_Area_2_AND_L_Area_3a_AND_L_Primary_Sensory',...
                  'R_Area_1_AND_R_Area_2_AND_R_Area_3a_AND_R_Primary_Sensory',...
                  'ctx_lh_G_cingul-Post-dorsal_AND_ctx_lh_G_cingul-Post-ventral',...
                  'Left-MDl_AND_Left-MDm',...
                  'Right-MDl_AND_Right-MDm',...
                  'Left-Basal-nucleus_AND_Left-Lateral-nucleus',...
                  'Right-Basal-nucleus_AND_Right-Lateral-nucleus'};
mergedROInames = append(mergedROInames,ROIformat);

if (length(ROIlist) ~= length(mergedROInames))
    error("Merged ROI names must contain ROI to merge");
end

%% Iterate through each subject on the list
for nr = 1:height(rows)
    sub = char(rows.sub(nr));
    ses = char(rows.ses(nr));
    
    subdir = fullfile(projPath,...
                      strcat('sub-',sub),...
                      strcat('ses-',ses));
    
    unzip(fullfile(subdir,'output','fs.zip'),fullfile(subdir,'output'));              
    
    ROIdir = fullfile(subdir,'output','fs','ROIs');
    niiFiles = dir(fullfile(ROIdir, '*.nii*'));
    
    ROIs = cell(1,length(niiFiles));
    for K = 1 : length(niiFiles)
      ROIs{K} = niiFiles(K).name;
    end
    
    % create tmpdirectory where to process ROI
    ROItmp = fullfile(subdir,'output','tmp','ROIs');
    if ~exist(ROItmp, 'dir')
        mkdir(ROItmp)
    end
    
    for rl = 1:numel(ROIlist)
        ROIs2merge = ROIlist{rl};
        if ~isequal(sum(contains(ROIs,ROIlist{rl})), numel(ROIs2merge))
            error('Missing some of these ROIs:\n %s\n',ROIs2merge{:})
        end
        
        % NIFTI to .mat & combine ROIs
        % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
        roiCombined = [];
        for j = 1:numel(ROIs2merge)
            % unzip it in ROItmp
            gunzip(fullfile(ROIdir, ROIs2merge{j}),ROItmp);
            % Load the NIfTI image
            niiFile = fullfile(ROItmp, strrep(ROIs2merge{j},'.gz',''));

            V = spm_vol(niiFile);
            Y = spm_read_vols(V);

            % Create a new region of interest (ROI) from the NIfTI image
            roi = maroi_image(struct('vol', V, 'binarize', 0.5));

            % Create the ROI filename with the same name as the NIfTI file
            [~, filename, ~] = fileparts(niiFile);
            roiFile = fullfile(ROItmp, [filename '.mat']);
            % Save the ROI as a separate file
            save(roiFile, 'roi');
            
            if isempty(roiCombined)
                roiCombined = roi;
            else
                roiCombined = roiCombined | roi;
            end
        end
        
        % Set the label and description for the combined ROI
        combinedROIname = extractBefore(mergedROInames{rl},'.');
        roiCombined = label(roiCombined, combinedROIname);
        roiCombined = descrip(roiCombined, combinedROIname);
        
        % Save combined ROIs into .mat and nifti
        % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
        % Save the combined ROI as a separate file
        combinedRoiFile = fullfile(ROItmp, [combinedROIname '.mat']);
        roiCombined = saveroi(roiCombined, combinedRoiFile);
        
        % Save it as nifti (both decompressed and compressed)
        outputFileName = fullfile(ROItmp, [combinedROIname, '.nii']);
        % Save the ROI as a NIfTI image using the same name
        save_as_image(roiCombined, outputFileName);
        gzip(outputFileName);
    end
    copyfile(fullfile(ROItmp,'*.nii.gz'),ROIdir);
end