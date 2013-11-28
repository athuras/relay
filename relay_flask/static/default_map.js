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
		bounds.minlat = b.getSouth();
		bounds.minlong = b.getWest();
		bounds.maxlat = b.getNorth();
		bounds.maxlong = b.getEast();
	}
	$.ajax({
		type: "POST",
		datatype: "JSON",
		url: "http://localhost:5000/request_intersections",
		data: JSON.stringify(bounds),
		async: false,
		success: function(){
			console.log(data);
			// prepData(data);
		},
		error: function(){
			console.log('request for data failed');
		}
	});
}

// create map
var map = L.map('map').setView([43.673513374000002, -79.573719920000002], 14);

// add tile layer
L.tileLayer('http://{s}.tile.cloudmade.com/e440fa3faa334156831adb28596d54a0/115014/256/{z}/{x}/{y}.png', {
	maxZoom: 18,
	attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="http://cloudmade.com">CloudMade</a>'
}).addTo(map);

// Make a single popup
var popup = L.popup();

for(inter in intersections){
	if(intersections.hasOwnProperty(inter)){
		var i = intersections[inter];

		// background marker
		L.circleMarker([i['x'], i['y']], {
			stroke: false,
			fill: true,
			fillOpacity: 0.2,
			radius: i['volume']/2,

			clickable: false
		})
			.addTo(map);

		// action marker
		L.circleMarker([i['x'], i['y']], {
			id: i['id'],
			stroke: false,
			fill: true,
			fillOpacity: 0.9,
			radius: 10,

			clickable: true
		})
		.addTo(map)
		.on('click',populatePopupForIntersection);
	}
}

function populatePopupForIntersection(event){
	intersection = getIntersectionById(event.target.options.id);

	var content = "";
	content += "<h1>" + intersection.title + "</h1>";
	content += "<a onclick='populatePanelForIntersection(" + intersection.id + ")'>more</a>";

	var latlng = L.latLng(intersection.x, intersection.y);

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

	$('#intersection-title').html(intersection['title']);
	$('#intersection-performance').html(String(intersection['performance']));
}

function getIntersectionById(id){
	// find the item in the array with this ID
	var intersection = $.grep(intersections, function(i){ return i.id == id});
	intersection = intersection[0]; //only one?
	return intersection;
}



