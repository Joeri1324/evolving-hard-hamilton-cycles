var canvasSelection = d3.select("#plotCanvas");
var canvasElement = document.getElementById("plotCanvas");
var data, svg, xMap, yMap, averageLine, medianLine, tooltip;
var bucketedResults = {};
var graphs = {};

//Define the color range for data points
var	color = d3.scaleOrdinal().domain(["true", "false", "null"]).range(["#D81B60", "#2979FF", "#000000"]);

var startTime = new Date();
// Promise.all([
//     d3.json("../results/iterations/indexed-16-node-test-set.json"),
//     // d3.json("jsondata/indexed-24-node-test-set.json"),
//     // d3.json("jsondata/results-cetal-16.json"),
//     // d3.json("jsondata/results-cetal-24.json"),
//     // d3.json("jsondata/results-horn-16.json"),
//     // d3.json("jsondata/results-horn-24.json"),
//     // d3.json("jsondata/results-vacul-16.json"),
//     // d3.json("jsondata/results-vacul-24.json")
// ]).then(function(files) {
// 	processGraphs(files[0]);
// 	processGraphs(files[1]);
// 	files[0].concat(files[1]).forEach(function(graph){
// 		graphs[graph["identifier"]] = graph;
// 	});

// 	processResults(files[2], 16);
// 	processResults(files[3], 24);
// 	processResults(files[4], 16);
// 	processResults(files[5], 24);
// 	processResults(files[6], 16);
// 	processResults(files[7], 24);
// 	data = files;
// 	processBucketedResults();

// 	let endTime = new Date();
// 	console.log("Loading and preparing files took: " + (endTime - startTime) / 1000 + " seconds.");
// 	redraw();
// 	var event = document.createEvent("HTMLEvents");
// 	event.initEvent('change', false, true);
// 	let initialDataCheckbox = document.getElementById("initialDataCheckbox");
// 	initialDataCheckbox.checked = true;
// 	initialDataCheckbox.dispatchEvent(event);
// 	handleTracking();
// }).catch(function(err) {
//     console.log("error loading data", err)
// })


const loadGraphs = async (file) => {
  return d3.json(file)
    .then(data => {
      processGraphs(data);
      data.forEach(function(graph) {
        graphs[graph["identifier"]] = graph;
      });
      console.log('!!', "finished");
    })
    .catch(console.error)
}

let paths = {}

const loadResults = async (file) => {
  return d3.json(file)
    .then(data => {
			data.forEach(d => {
				paths[d['id']] = d['path'];
			});
      processResults(data, 32);
      processBucketedResults();
      drawDataPoints(data, "cetal graphsize16 data");
    })
    .catch(console.error)
}

const run = async () => {
	while(canvasElement.firstChild){
		canvasElement.removeChild(canvasElement.firstChild);
	}
 	await loadGraphs("resources/indexed-16-node-test-set.json");
 	canvasSetup(32);
	await loadResults("results/iterations/result-checkallwithpruning-9.json");

	console.log('BUCKET', bucketedResults);
	console.log("graphss", paths);

	// d3.json(`resources/indexed-16-node-test-set.json`)
	// 	.then(d => {
			
	// 		// 1022
	// 		// 843
	// 		drawForceDirectedGraph(d[843], null) // paths['1210'])
			
	// 	});
	d3.json('../results/hillclimb-hc/10000-evaluations/12-size/1/9997.json')
		.then(d => {
			console.log("SSSSSSSSSSS", d)
			drawForceDirectedGraph(d, d["path"])
		})

	d3.select("#plotCanvas").selectAll("circle").on('click', function(d) {
		dataClickHandler(d);
	})
}

function dataClickHandler(dataPoint) {
	console.log("!!!!!", dataPoint);
	let matches = d3.select("#plotCanvas").selectAll("circle").filter(function(d, i) { return d.id === dataPoint.id; });
	let lastHighlighted = d3.selectAll(".lastHighlighted");
	if(lastHighlighted.empty()){
		lastHighlighted = matches;
		highlight(matches);
	}else{
		unhighlight(lastHighlighted);
		lastHighlighted = matches;
		highlight(matches)
	}
	// ITERATIONS !,15682491
	clearForceDirectedGraph();
	d3.json(`resources/indexed-32-node-test-set/${dataPoint['id']}.json`)
		.then(g => {
			drawForceDirectedGraph(g, dataPoint["path"]);
		});
}

run();





function redraw(){
	let startTime = new Date();
	while(canvasElement.firstChild){
		canvasElement.removeChild(canvasElement.firstChild);
	}
	canvasSetup();
	drawDataPoints(data[2], "cetal graphsize16 data");
	drawDataPoints(data[3], "cetal graphsize24 data");
	drawDataPoints(data[4], "horn graphsize16 data");
	drawDataPoints(data[5], "horn graphsize24 data");
	drawDataPoints(data[6], "vacul graphsize16 data");
	drawDataPoints(data[7], "vacul graphsize24 data");
	drawLine("cetal", 16, "cetal average graphsize16", averageLine);
	drawLine("cetal", 16, "cetal median graphsize16", medianLine);
	drawLine("cetal", 24, "cetal average graphsize24", averageLine);
	drawLine("cetal", 24, "cetal median graphsize24", medianLine);
	drawLine("horn", 16, "horn average graphsize16", averageLine);
	drawLine("horn", 16, "horn median graphsize16", medianLine);
	drawLine("horn", 24, "horn average graphsize24", averageLine);
	drawLine("horn", 24, "horn median graphsize24", medianLine);
	drawLine("vacul", 16, "vacul average graphsize16", averageLine);
	drawLine("vacul", 16, "vacul median graphsize16", medianLine);
	drawLine("vacul", 24, "vacul average graphsize24", averageLine);
	drawLine("vacul", 24, "vacul median graphsize24", medianLine);
	let endTime = new Date();
	console.log("Redraw took: " + (endTime - startTime)/1000 + " seconds.");
}

function processGraphs(file){
	file.forEach(function(graph, index){
		graph["identifier"] = "n" + graph["vertices"].length + "-" + graph["identifier"];
	});
}

function processResults(file, graphSize){
	console.log("!!!!")
	file.forEach(function(result, index){
		// result["graphID"] = "n" + graphSize + "-" + result["id"];
		result["relativeCost"] = result["relativeCost"] === 0 ? 1 / graphs[result["graphID"]]["size"] : result["relativeCost"];
		switch (result["algorithm"]) {
			case 'CHEESEMAN':
				result["algorithm"] = "cetal";
				break;
			case 'INVERSECHEESEMAN':
				result["algorithm"] = "horn";
				break;
			case 'PRUNING':
				result["algorithm"] = "vacul";
				break;
		}

    let graph = graphs["n" + graphSize + "-" + result["id"]];
		let averageConnectivity = result['degree'];

		if(bucketedResults.hasOwnProperty(result["algorithm"])){
			let algorithmBucket = bucketedResults[result["algorithm"]];
		
			if(algorithmBucket.hasOwnProperty(graphSize)){
				let graphSizeBucket = algorithmBucket[graphSize];
		
				if(graphSizeBucket.hasOwnProperty(averageConnectivity)){
					graphSizeBucket[averageConnectivity].push(result["iterations"]);
				}else{
					graphSizeBucket[averageConnectivity] = [result["iterations"]];	
				}				
			}else{
				let relativeCostList = [result["iterations"]];
				let graphSizeBucket = {};
				graphSizeBucket[averageConnectivity] = relativeCostList;
				algorithmBucket[graph["size"]] = graphSizeBucket;
			}
		}else{
			let relativeCostList = [result["iterationst"]];
			let graphSizeBucket = {};
			graphSizeBucket[averageConnectivity] = relativeCostList;
			let algorithmBucket = {};
			algorithmBucket[graphSize] = graphSizeBucket;
			bucketedResults[result["algorithm"]] = algorithmBucket;
		}
	});
}

function processBucketedResults() {
	for (algorithm in bucketedResults){
		let algorithmBucket = bucketedResults[algorithm];
		for(size in algorithmBucket){
			let sizeBucket = algorithmBucket[size];
			for(averageConnectivity in sizeBucket){
				let averageConnectivityBucket = sizeBucket[averageConnectivity];
				let sum = calcSum(averageConnectivityBucket);
				let average = sum/averageConnectivityBucket.length;
				let median = calcMedian(averageConnectivityBucket);
				averageConnectivityBucket["average"] = average;
				averageConnectivityBucket["median"] = median;
				averageConnectivityBucket["averageConnectivity"] = averageConnectivity;
			}
		}
	}
}

function canvasSetup(graphSize) {
	canvasSelection = d3.select("#plotCanvas");
	canvasElement = document.getElementById("plotCanvas");
		//Set the width and height to the size of the containing div (#d3canvas)
	var margin = {top: 40, right: 40, bottom: 40, left: 70},
		width = canvasElement.getBoundingClientRect().width - margin.left - margin.right;
		height = canvasElement.getBoundingClientRect().height - margin.top - margin.bottom;

	//Set up the svg plot area
	svg = canvasSelection.append("svg")
			.attr("width", width + margin.left + margin.right)
			.attr("height", height + margin.top + margin.bottom)
		.append("g")
			.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

	//Set up the tooltip
	tooltip = canvasSelection.append("div")
		.attr("class", "tooltip");

	// Setup x (axis and scaling)
	var xPadding = 0,
		xValue = function(result) {
			return result['degree'];
		},
		xScale = d3.scaleLinear().range([xPadding, width - xPadding]).domain([0, graphSize]),
		xAxis = d3.axisBottom(xScale).ticks(graphSize);
	xMap = function(d) {return xScale(xValue(d));};
	
	// Setup y (axis and scaling) for regular data points
	var yPadding = 0,
		yValue = function(result) {
			console.log(result['degree'], result['iterations'])
			return result["iterations"];
		},
		yScale = d3.scaleLog().range([height - yPadding, yPadding]).domain([0.01, 100000000]).nice(),
		yAxis = d3.axisLeft(yScale)
			.ticks(11)
			.tickFormat(function(d) {
				return yScale.tickFormat(1, d3.format(".1e"))(d);
			});
	yMap = function(d) {return yScale(yValue(d));};

	// Define an averages line
	averageLine = d3.line()
	    .x(function(averageConnectivityBucket) { return xScale(parseFloat(averageConnectivityBucket["averageConnectivity"])); })
	    .y(function(averageConnectivityBucket) { return yScale(averageConnectivityBucket["average"]); });	
		
	// Define a median line
	medianLine = d3.line()
	    .x(function(averageConnectivityBucket) { return xScale(parseFloat(averageConnectivityBucket["averageConnectivity"])); })
	    .y(function(averageConnectivityBucket) { return yScale(averageConnectivityBucket["median"]); });

	//Draw x-axis
	svg.append("g")
		.attr("class", "axis")
		.attr("transform", "translate(0," + height + ")")
		.call(xAxis)
	
	svg.append("text")
		.attr("class", "label")
		.attr("x", width / 2)
		.attr("y", height)
		.attr("dy", "2em")
		.style("text-anchor", "middle")
		.text("Average connectivity");

	//Draw y-axis	
	svg.append("g")
		.attr("class", "axis")
		.call(yAxis)
	
	svg.append("text")
		.attr("class", "label")
		.attr("transform", "rotate(-90)")
		.attr("x", -height/2)
		.attr("y", "-3em")
		.style("text-anchor", "middle")
		.text("Relative cost (iterations / nodes)");
}

function drawDataPoints(resultsArray, className){
	console.log("!!!!!", resultsArray);
	svg.selectAll(className)
			.data(resultsArray)
		.enter().append("circle")
			.attr("data-graphid", function(result) {return result["graphID"];})
			.attr("class", className)
			.attr("r", 1.5)
			.attr("cx", xMap)
			.attr("cy", yMap)
			.style("fill", function(result) { return color(result["hamiltonian"]);})
			// .style("display", "none")
			.on("mouseover", dataMouseOver)
			.on("mouseout", dataMouseOut)
			.on("click", (d) => dataClickHandler(d));;
}

function drawLine(algorithm, graphSize, className, line){
	svg.append("path")
		.datum(Object.values(bucketedResults[algorithm][graphSize])
			.sort(function(a,b){
				return parseFloat(a["averageConnectivity"])-parseFloat(b["averageConnectivity"]);	
			}))
		.attr("d", line)
		.attr("class", className);
}

function toggleCheckbox(element) {
	let selector = "." + element.className.split(" ").join(".");
	let svgElements = canvasSelection.selectAll(selector);
	if(element.checked){
		svgElements.style("display", "block");
	}else{
		svgElements.style("display", "none");
	}
}


function toggleMenu(button){
	let menu = document.getElementById('plot-menu')
	menu.classList.toggle('collapsed');
	if(menu.classList.contains("collapsed")){
		button.innerHTML = "Open menu";
	}else{
		button.innerHTML = "Close menu";
	}
}

function createHistogram(graph){
	//Convert map string to object
	let mapObject = graph['connectivityMap'];
	
	//Collect the values of the properties in the object
	let keys = keysAsInt(mapObject);
	let values = valuesAsInt(mapObject);
	
	//Define the min and max values for the scales of the histogram
	let xMax = graph['size']-1,
		xMin = 0;
		yMin = 0,
		yMax = graph['size'];	
	
	//Formatter for the numbers on the bars
	let formatCount = d3.format(",.0f");
	
	//Define width and height of the histogram.
	let margin = {top: 40, right: 20, bottom: 40, left: 20},
		width = 500 - margin.left - margin.right,
		height = 150 - margin.top - margin.bottom;

	//Define the x and y scales.
	let xScale = d3.scaleLinear()
		.domain([0, xMax])
		.range([0, width])
	
	let yScale = d3.scaleLinear()
		.domain([0, yMax])
		.range([height, 0]);
	
	//Define the x axis
	let xAxis = d3.axisBottom()
		.scale(xScale)
		.ticks(keys.length-1);
	
	//Put the values in a d3 histogram dataformat
	let histogram = d3.histogram(values)
			.domain(xScale.domain())
			.thresholds(xScale.ticks(xMax));
			
	//Create the histogram tooltip draw area
	let histSVG = tooltip.append("svg")
		.attr("class", "tooltip-canvas")		
		.attr("width", width + margin.left + margin.right)
		.attr("height", height + margin.top + margin.bottom)
		.attr("transform", "translate(" + margin.left + "," + margin.top + ")");
	
	//append the bar placeholders to the draw area
	let bar = histSVG.selectAll(".bar")
		.data(histogram(keys))
		.enter().append("g")
			.attr("class", "histogram-bar")
			.attr("transform", function(d) { return "translate(" + xScale(d.x0-0.5) + "," + yScale(d.length) + ")"; });
	
	//append the rectangles to the bar placeholders
	bar.append("rect")
		.attr("x", 1)
		.attr("width", xScale(.95))
		.attr("height", function(d) { return height - yScale(d.length); })
		.attr("class", "histogram-bar-rect");

	//Append the counts to the bars
	bar.append("text")
		.attr("dy", "0.75em")
		.attr("y", -13)
		.attr("x", xScale(0.5))
		.attr("text-anchor", "middle")
		.text(function(d) { return d.length ? formatCount(d.length) : ""; })
		.attr("class", "histogram-bar-label");

	//Append the axis to the tooltip histogram draw area
	histSVG.append("g")
		.attr("class", "histogram-x-axis")
		.attr("transform", "translate(0," + height + ")")
		.call(xAxis)

	histSVG.append("text")
		.attr("class", "histogram-axis-label")
		.attr("text-anchor", "middle")
		.attr("x", width/2)
		.attr("y", height)
		.attr("dy", "2em")
		.text("Degree");
}

//Create histogram
function dataMouseOver(d) {
	tooltip.transition()
		   .duration(200)
		   .style("opacity", .9);
		tooltip.html("<p>Node count of degree</p>")
		   .style("left", (d3.mouse(d3.select('#plotCanvas').node())[0]) + "px")
			 .style("top", (d3.mouse(d3.select('#plotCanvas').node())[1]) + "px");
			 
	d3.json(`resources/indexed-32-node-test-set/${d['id']}.json`)
		.then(createHistogram)

	//(graphs[d["graphID"]]);
}

//Fade out histogram
function dataMouseOut(d){
	tooltip.transition()
		.duration(300)
		.style("opacity", 0);
}

//Handle click on a data point


//Highlight some selection of data points
function highlight(selection){
	if(!selection.empty()){
		selection.style("fill", "#ff8000")
				.style("opacity", 1)
				.attr("r", 7)
				.classed("lastHighlighted", true);
	}
}

//Unhighlight some selection of data points
function unhighlight(selection){
	if(!selection.empty()){
		selection.style("fill", function(d) { return color(d["hamiltonian"]);})
				.style("stroke", "none")
				.style("opacity", .5)
				.attr("r", 3.5)
				.classed("lastHighlighted", false);
	}
}

function valuesAsInt(object) {
  var values = [];
  for(var property in object) {
    values.push(+object[property]);
  }
  return values;
}

function keysAsInt(object) {
  var keys = [];
  for(var key in object) {
	if(object.hasOwnProperty(key)){
		keys.push(+object[key]);
	}
  }
  return keys;
}

function calcSum(list){
	let sum = 0.0;
	for(let i=0; i<list.length; i++){
		sum = sum + parseFloat(list[i]);
	}
	return sum;
}

function calcMedian(values){
  values.sort(function(a,b){
    return a-b;
  });

  if(values.length ===0) return 0

  var half = Math.floor(values.length / 2);

  if (values.length % 2)
    return values[half];
  else
    return (values[half - 1] + values[half]) / 2.0;
}

Array.prototype.max = function() {
  return Math.max.apply(null, this);
};

Array.prototype.min = function() {
  return Math.min.apply(null, this);
};


