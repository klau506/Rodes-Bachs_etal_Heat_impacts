var region = ee.Geometry.Rectangle([-22, 34, 32, 72]);
var dailyLST = ee.ImageCollection("MODIS/061/MOD11A1")
    .filterBounds(region)
    .filterDate('2023-01-01', '2024-01-01')
    .select('LST_Day_1km');

// 1. Create a list of Start Days (1, 9, 17, 25...)
var startDays = ee.List.sequence(1, 365, 8);

// 2. Map over the start days to create 8-day mean images
var images8Day = startDays.map(function(day) {
  var start = ee.Date('2023-01-01').advance(ee.Number(day).subtract(1), 'day');
  var end = start.advance(8, 'day');
  
  // Create a clean date label for the filename
  var label = start.format('YYYY_MM_dd');
  
  var mean = dailyLST.filterDate(start, end).mean();
  
  // Scaling to Celsius
  var celsius = mean.multiply(0.02).subtract(273.15);
  
  return celsius.set({
    'system:time_start': start.millis(),
    'label': label
  });
});

// 3. Trigger the Exports
// We convert the list to a list of objects we can iterate through in JavaScript
var exportList = images8Day.getInfo(); 

exportList.forEach(function(imgInfo) {
  var img = ee.Image(images8Day.get(exportList.indexOf(imgInfo)));
  var dateStr = imgInfo.properties.label;
  
  Export.image.toDrive({
    image: img,
    description: 'LST_8Day_' + dateStr, // e.g., LST_8Day_2023_01_01
    folder: 'LST_Europe_2023_Series',
    // Instead of scale: 1000, we define the degree-based grid:
    // [xScale, xShearing, xTranslation, yShearing, yScale, yTranslation]
    crsTransform: [0.01, 0, 0, 0, -0.01, 0],   
    region: region,
    crs: 'EPSG:4326', // Standard European Projection for your R work
    maxPixels: 1e13
  });
});