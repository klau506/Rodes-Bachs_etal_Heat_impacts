// available from 2000-02-26 to 2020-05-24
// Update at 2020-09-11, Dongdong Kong
var imgcol_8d = ee.ImageCollection("projects/pml_evapotranspiration/PML/OUTPUT/PML_V2_8day_v017_ARC_061");

// convert all bands to 'float' to handle both Small (Byte) and Large (UInt16) numbers
imgcol_8d = imgcol_8d.map(function(img) {
  return img.toFloat();
});


/**
 * Copyright (c) 2019 Dongdong Kong. All rights reserved.
 * This work is licensed under the terms of the MIT license.
 * For a copy, see <https://opensource.org/licenses/MIT>.
 */
var pkg_export = require('users/kongdd/pkgs:pkg_export.js');
// var pkg_trend  = require('users/kongdd/public:Math/pkg_trend.js');
// export parameters
var options = {
    type: "drive",
    // Updated to European lon-lat limits: [West, South, East, North]
    range: [-22, 34, 32, 72], // original  range: [-180, -60, 180, 90], 
    cellsize: 1/100,
    // crsTransform : [463.312716528, 0, -20015109.354, 0, -463.312716527, 10007554.677], // prj.crsTransform;
    // scale        : 463.3127165275, // prj.scale
    crs: 'EPSG:4326', // 'SR-ORG:6974', // EPSG:4326
    folder: 'PMLV2'
};

imgcol_8d = imgcol_8d.filterDate('2023-01-01', '2025-01-01');
print('latest:', imgcol_8d.filterDate('2023-01-01', '2025-01-01'));
pkg_export.ExportImgCol(imgcol_8d, 'PMLV2_latest', options);