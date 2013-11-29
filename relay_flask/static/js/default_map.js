// some sample data:
var intersections = [
	{
		"id":13463459,
		"y":-79.576072851999996,
		"x":43.672620101,
		"title": "Eglinton E Ramp / 427 X N 401 X E Ramp",
		"performance": 100,
		"volume": 75
	},
	{
		"id":13463450,
		"y":-79.573719920000002,
		"x":43.673513374000002,
		"title": "427 C N 27 N Ramp / Eglinton W 427 C S Ramp",
		"performance": 50,
		"volume": 25
	}
];

// Make a single popup
var popup = L.popup();

// create map
var map = L.map('map').setView([43.617252641,-79.378593649], 13);
map.on('click', hidePanelForIntersection);

// add tile layer
L.tileLayer('http://{s}.tile.cloudmade.com/e440fa3faa334156831adb28596d54a0/115014/256/{z}/{x}/{y}.png', {
	maxZoom: 18,
	attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="http://cloudmade.com">CloudMade</a>'
}).addTo(map);

// init function
function init(){
	//sample bounds
	var bounds = L.latLngBounds();
	getIntersections(bounds);
}

//accepts bounds in form of L.latLngBounds
function getIntersections(b){
	//{"minlat": 43.61,"maxlat": 43.62, "minlong": -79.39, "maxlong": -79.37}
	var bounds = new Object();
	if(b == null){
		//get all
		bounds.minlat = -90;
		bounds.minlong = -180;
		bounds.maxlat = 90;
		bounds.maxlong = 180;
	} else {
		bounds.minlat = b.getSouth();
		bounds.minlong = b.getWest();
		bounds.maxlat = b.getNorth();
		bounds.maxlong = b.getEast();
	}
	$.when(
		$.ajax({
			type: "POST",
			datatype: "JSON",
			contentType: "application/json",
			url: "http://localhost:5000/request_intersections",
			data: JSON.stringify(bounds),
			async: false
        })
	).then( function(data){
		//passed
		console.log('passed!');
		intersections = prepData(data);
		populateMap(intersections);
	}, function(error){
		//failed
		console.log('shit :(');
		console.log(error);
	});
}

function prepData(data){
	// Add some bogus data to them!
	for(i in data){
		if(data.hasOwnProperty(i)){
			data[i].id = Math.floor((Math.random()*10000));

			var stat = Math.random();
			data[i].online = (stat < 0.05)? false : true;
			data[i].status = (stat < 0.05)? "MAINTENANCE" : "OK";

			data[i].volume = Math.random();
			data[i].performance = Math.random();
		}
	}
	return data;
}

function populateMap(intersections) {
	var oColor, oRadius, oOpacity;
	var iColor, iRadius, iOpacity;
	var mColor, mRadius, mOpacity;
	for(inter in intersections){
		if(intersections.hasOwnProperty(inter)){
			var i = intersections[inter];

			// oColor = '#fff';
			// oRadius = 20;
			// oOpacity = 0.05;

			// iColor = '#fff';
			// iRadius = 5;
			// iOpacity = 0.7;

			// decide what happens to the point!
			if(i.online){
				oColor = '#fff';
				oRadius = iRadius + Math.floor(30*i.volume);
				oOpacity = 0.05;

				mColor = '#fff';
				mRadius = iRadius + Math.floor(20*i.volume);
				mOpacity = 0.1;

				if(i.performance >= 0.5){
					iColor = '#fff';
				} else {
					iColor = '#ff3a3a';
				}
				iRadius = 10;
				iOpacity = 0.12;
			} else {
				oColor = '#000';
				oRadius = 20;
				oOpacity = 0.1;

				mColor = '#000';
				mRadius = 15;
				mOpacity = 0.15;

				iColor = '#000';
				iRadius = 10;
				iOpacity = 0.2;
			}

			// background marker
			L.circleMarker([i['lat'], i['long']], {
				stroke: false,
				fill: true,
				clickable: false,

				fillOpacity: oOpacity,
				radius: oRadius,
				color: iColor
			})
				.addTo(map);

			// middle marker
			L.circleMarker([i['lat'], i['long']], {
				stroke: false,
				fill: true,
				clickable: true,

				fillOpacity: mOpacity,
				radius: mRadius,
				color: iColor
			})
			.addTo(map)

			// action marker
			L.circleMarker([i['lat'], i['long']], {
				id: i['id'],
				stroke: false,
				fill: true,
				clickable: true,

				fillOpacity: iOpacity,
				radius: iRadius,
				color: iColor
			})
			.addTo(map)
			.on('click',populatePopupForIntersection);
		}
	}
}

function returnColor(hex){
	return function(){
		return hex;
	}
}

function populatePopupForIntersection(event){
	intersection = getIntersectionById(event.target.options.id);

	var content = "";
	var title = (intersection.name == "")? '(Untitled)' : intersection.name;
	content += "<h1>" + title + "</h1>";
	content += "<a onclick='populatePanelForIntersection(" + intersection.id + ")'>more</a>";

	var latlng = L.latLng(intersection.lat, intersection.long);

	var popup = L.popup()
	    .setLatLng(latlng)
	    .setContent(content)
	    .openOn(map);
}

function populatePanelForIntersection(id){
	console.log('populate panel for id ' + String(id));

	// find the item in the array with this ID
	var intersection = $.grep(intersections, function(i){ return i.id == id});
	intersection = intersection[0];

	// populate the text fields with the intersection's data
	$('#intersection-name').html(intersection.name);
	$('#intersection-performance').html(String(Math.floor(intersection.performance*100))+ "%");
	$('#intersection-volume').html(String(Math.floor(intersection.volume*100)) + "%");

	// Charts.
	// do the AJAX call for the chart datatype

	//for now, bull data
	var data = {
		labels : ["January","February","March","April","May","June","July"],
		datasets : [
			{
				fillColor : "rgba(220,220,220,0.5)",
				strokeColor : "rgba(220,220,220,1)",
				pointColor : "rgba(220,220,220,1)",
				pointStrokeColor : "#fff",
				data : [65,59,90,81,56,55,40]
			},
			{
				fillColor : "rgba(151,187,205,0.5)",
				strokeColor : "rgba(151,187,205,1)",
				pointColor : "rgba(151,187,205,1)",
				pointStrokeColor : "#fff",
				data : [28,48,40,19,96,27,100]
			}
		]
	}

	// populate the charts
	var ctx = document.getElementById("performance-chart").getContext("2d");
	var performanceChart = new Chart(ctx).Line(data);

	// finally, show the panel
	$('#intersection-details').css("display", "block");
}

function getIntersectionById(id){
	// find the item in the array with this ID
	var intersection = $.grep(intersections, function(i){ return i.id == id});
	intersection = intersection[0]; //only one?
	return intersection;
}

function hidePanelForIntersection(e){
	$('#intersection-details').css("display", "none");
}



