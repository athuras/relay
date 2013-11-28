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

// create map
var map = L.map('map').setView([43.673513374000002, -79.573719920000002], 10);

// add tile layer
L.tileLayer('http://{s}.tile.cloudmade.com/e440fa3faa334156831adb28596d54a0/115014/256/{z}/{x}/{y}.png', {
	maxZoom: 18,
	attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="http://cloudmade.com">CloudMade</a>'
}).addTo(map);

// Make a single popup
var popup = L.popup();


// add all the markers to the map
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
			stroke: false,
			fill: true,
			fillOpacity: 0.9,
			radius: 10,

			clickable: true
		})
			.addTo(map)
			// .bindPopup("<p>" + i['title'] + "</p>");
			// .on('click',onIntersectionClick(i));
			.on('click', function(e, i){
				return function(i){
					popup
						.setLatLng(L.latLng(i['x'], i['y']))
						.setContent(i.title)
						.openOn(map);
				}
			})
	}
}

// When an intersection is clicked on, serve the popup.
// function onIntersectionClick(intersection) {
// 	return function(intersection){
// 		popup
// 			.setLatLng(intersection.latlng)
// 			.setContent(intersection.title)
// 			.openOn(map);
// 	}
// }


