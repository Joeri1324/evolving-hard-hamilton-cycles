var graphMargin = {top: 20, right: 20, bottom: 20, left: 20},
	graphWidth = document.getElementById("graphCanvas").getBoundingClientRect().width,
	graphHeight = document.getElementById("graphCanvas").getBoundingClientRect().height,
	force, svg2;

graphCanvasSetup();
//Set up the svg graph area
function graphCanvasSetup(){
	svg2 = d3.select("#graphCanvas").append("svg")
				.attr("width", graphWidth)
				.attr("height", graphHeight)
			.append("g")
				.attr("transform", "translate(" + graphMargin.left + "," + graphMargin.top + ")");
}
			
function drawForceDirectedGraph(graph, hamiltonCycle){
	//Evaluate the connectivity map as JSON
	let degreeMap = graph["connectivityMap"];
	
	//Initialize all the nodes
	let nodes = [];
	for(i=0; i<graph.vertices.length; i++){
		nodes.push({x: graphWidth/(i+1), y:graphHeight/(i+1), degree:degreeMap[i], id:i});
	}

	//If there is an HC:
	let hamiltonSources = [];
	let hamiltonTargets = [];
	if(hamiltonCycle){
		for(i=0; i<hamiltonCycle.length-1; i++){
			hamiltonSources.push(hamiltonCycle[i]);
			hamiltonTargets.push(hamiltonCycle[i+1]);
		}
		hamiltonSources.push(hamiltonCycle[0])
		hamiltonTargets.push(hamiltonCycle[length])	
	}

	//Initialize all edges
	let edges = graph.edges;
	let links = [];
	for(i=0; i<edges.length; i++){
		let source = edges[i].endpoints[0].id;
		let target = edges[i].endpoints[1].id;
		let sourceIndex = hamiltonSources.indexOf(source);
		let sourceIndex2 = hamiltonTargets.indexOf(source);
		let targetIndex = hamiltonTargets.indexOf(target);
		let targetIndex2 = hamiltonSources.indexOf(target);
		if(sourceIndex != -1 && (sourceIndex === targetIndex || sourceIndex2 === targetIndex2)){
			links.push({source:source, target:target, id:i, type:"hc"});
		}else{
			links.push({source:source, target:target, id:i, type:"regular"});
		}
	}


	
	//Define a color scale based on degree of a node
	let nodecolor = d3.scaleLinear()
					.domain([0, 23])
					.range(["white", "#b30000"]);
	
	//Define color scale based on the type of link
	let edgecolor = d3.scaleOrdinal()
					.domain(["regular", "hc"])
					.range(["gray", "#33cc33"]);

	//Define and start the force layout
	force = d3.forceSimulation(nodes)
			.force("charge", d3.forceManyBody().strength(-1000))
			.force("link", d3.forceLink(links).distance(200).id(d => d.id))
			// .size([graphWidth, graphHeight])
			.force('center', d3.forceCenter(graphWidth / 2, graphHeight / 2))
			.force("x", d3.forceX())
  			.force("y", d3.forceY())
			.alphaTarget(1);

	//Enter all links
	let linkage = svg2.selectAll('.link')
		.data(links)
		.enter().append('line')
		.attr('class', 'link')
		.attr('stroke', function(d){return edgecolor(d.type)});
	
	//Enter all the nodes
	let node = svg2.selectAll(".node")
				.data(nodes)
			.enter().append("g")
				.attr("class", "node");

	node.call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended));
	
	//Append circles to the nodes
	node.append("circle")
			.attr('class', 'nodeCircle')
			.attr('r', 20)
			.style("fill", function(d) { return nodecolor(d["degree"]);})
			.on('dblclick', dblclick);

	//Append labels to the nodes
	node.append("text")
			.attr("text-anchor", "middle")
			.attr("dy", ".4em")
			.text(function(d){return d.degree});

	//Define animation of the layout.
	force.on('tick', function(){
		linkage.attr('x1', function(d) { return d.source.x; })
			.attr('y1', function(d) { return d.source.y; })
			.attr('x2', function(d) { return d.target.x; })
			.attr('y2', function(d) { return d.target.y; });
		node.attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });		
	});	
}

function dblclick(d) {
  dragended(d);
}

function dragstarted(d) {
  if (!d3.event.active) force.alphaTarget(0.3).restart();
  d.fx = d.x;
  d.fy = d.y;
}

function dragged(d) {
  d.fx = d3.event.x;
  d.fy = d3.event.y;
}

function dragended(d) {
  if (!d3.event.active) force.alphaTarget(0);
}

function clearForceDirectedGraph(){
	d3.select("#graphCanvas").selectAll('.link').remove();
	d3.select("#graphCanvas").selectAll('.hcLink').remove();
	d3.select("#graphCanvas").selectAll('.node').remove();
}
