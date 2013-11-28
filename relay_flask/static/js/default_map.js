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
var map = L.map('map').setView([43.617252641,-79.378593649], 14);
map.on('click', hidePanelForIntersection);

// add tile layer
L.tileLayer('http://{s}.tile.cloudmade.com/e440fa3faa334156831adb28596d54a0/115014/256/{z}/{x}/{y}.png', {
	maxZoom: 18,
	attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="http://cloudmade.com">CloudMade</a>'
}).addTo(map);

// init function
function init(){
	//sample bounds
	var bounds = L.latLngBounds(L.latLng(43.61, -79.39), L.latLng(43.62, -79.37));
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
		bounds.minlat = 	b.getSouth();
		bounds.minlong = b.getWest();
		bounds.maxlat = b.getNorth();
		bounds.maxlong = b.getEast();
	}
	var boundsArray = [bounds];
	var string = JSON.stringify(boundsArray);
	$.when(
		$.ajax({
			type: 'POST',
			datatype: 'JSON',
			url: 'http://localhost:5000/request_intersections',
			data: string,
			async: false
		})
	).then( function(data){
		//passed
		console.log('passed!');
		intersections = prepData(data);
		populateMap();
	}, function(error){
		//failed
		console.log('shit :(');
		console.log(error);
		var data = [{"lat": 43.617252641, "long": -79.378593649, "name": ""}, {"lat": 43.615640333, "long": -79.379787026, "name": "Lagoon Rd / Centre Island Pk"}, {"lat": 43.616748867, "long": -79.381405861, "name": "Centre Island Pk"}, {"lat": 43.615644162, "long": -79.383859851, "name": "Centre Island Pk"}, {"lat": 43.615531685, "long": -79.385420298, "name": ""}, {"lat": 43.616634859, "long": -79.3843377, "name": "Centre Island Pk"}, {"lat": 43.616556566, "long": -79.384080588, "name": "Centre Island Pk"}, {"lat": 43.617642375, "long": -79.385550662, "name": "Centre Island Pk"}, {"lat": 43.619474279, "long": -79.385665726, "name": ""}, {"lat": 43.614982991, "long": -79.38971764, "name": "Beach Rd / Lakeshore Ave"}, {"lat": 43.615058259, "long": -79.387234747, "name": ""}, {"lat": 43.613189796, "long": -79.386956572, "name": "Beach Rd / Lakeshore Ave"}, {"lat": 43.612246478, "long": -79.388877878, "name": "Beach Rd"}, {"lat": 43.613139077, "long": -79.382615663, "name": "Lakeshore Ave"}, {"lat": 43.613987775, "long": -79.379351345, "name": "Lakeshore Ave / Centre Island Pk"}, {"lat": 43.617412954, "long": -79.375421729, "name": "The Mall Ter"}, {"lat": 43.615598892, "long": -79.375206659, "name": "Lakeshore Ave / The Mall Ter"}, {"lat": 43.6169786, "long": -79.372422412, "name": "Lakeshore Ave / Avenue Of The Islands"}, {"lat": 43.61640393, "long": -79.3719358, "name": "Avenue Of The Islands"}, {"lat": 43.619214799, "long": -79.374476208, "name": "Avenue Of The Islands"}, {"lat": 43.619214799, "long": -79.374476208, "name": "Avenue Of The Islands"}, {"lat": 43.618969922, "long": -79.37421219, "name": "Avenue Of The Islands"}, {"lat": 43.618969922, "long": -79.37421219, "name": "Avenue Of The Islands"}, {"lat": 43.618529739, "long": -79.373837027, "name": "Cibola Ave / Avenue Of The Islands"}, {"lat": 43.61818324, "long": -79.374574654, "name": "Lagoon Rd / Cibola Ave"}, {"lat": 43.617682354, "long": -79.375640868, "name": "Cibola Ave / The Mall Ter"}, {"lat": 43.617815333, "long": -79.376242282, "name": "The Mall Ter"}, {"lat": 43.617750181, "long": -79.377052122, "name": "Lagoon Rd / The Mall Ter"}, {"lat": 43.61992857, "long": -79.375129707, "name": "Island Park Trl / Avenue Of The Islands"}, {"lat": 43.613224114, "long": -79.38333629, "name": "Lakeshore Ave"}, {"lat": 43.617139201, "long": -79.38028221, "name": "Centre Island Pk"}, {"lat": 43.618014649, "long": -79.385084061, "name": "Centre Island Pk"}, {"lat": 43.616752557, "long": -79.37319924, "name": "Lagoon Rd / Lakeshore Ave / The Mall Ter"}, {"lat": 43.617519421, "long": -79.375987695, "name": "Cibola Ave / The Mall Ter"}, {"lat": 43.617249165, "long": -79.375742401, "name": "The Mall Ter"}, {"lat": 43.619472434, "long": -79.3737402, "name": "Island Park Trl"}];
		intersections = prepData(data);
		populateMap(intersections);
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

				if(i.performance >= 0.5){
					iColor = '#0f0';
				} else {
					iColor = '#f00';
				}
				iRadius = 10;
				iOpacity = 0.7;
			} else {
				oColor = '#000';
				oRadius = 20;
				oOpacity = 0.2;

				iColor = '#000';
				iRadius = 10;
				iOpacity = 0.9;
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
	intersection = intersection[0]; //only one?

	$('#intersection-name').html(intersection.name);
	$('#intersection-performance').html(String(Math.floor(intersection.performance*100))+ "%");
	$('#intersection-volume').html(String(Math.floor(intersection.volume*100)) + "%");

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



