var bootstrap = new Object();
bootstrap.mapLayers =[
	{
		'title': 'Dark',
		'id': 'layer-one',
		'mapStyle': 'light',
		'markerStyle': 'red',
		'styleArray': [
	        {
	          "featureType": "administrative.locality",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#a0a0a0" }
	          ]
	        },{
	          "featureType": "administrative.province",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#b4b4b4" }
	          ]
	        },{
	          "featureType": "water",
	          "elementType": "geometry.fill",
	          "stylers": [
	            { "color": "#0b0b0b" }
	          ]
	        },{
	          "featureType": "landscape",
	          "elementType": "geometry.fill",
	          "stylers": [
	            { "color": "#313131" }
	          ]
	        },{
	          "featureType": "poi",
	          "elementType": "geometry",
	          "stylers": [
	            { "color": "#212121" }
	          ]
	        },{
	          "featureType": "poi",
	          "elementType": "labels",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "poi",
	          "elementType": "geometry.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "road",
	          "elementType": "geometry.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "road",
	          "elementType": "geometry.fill",
	          "stylers": [
	            { "color": "#171717" }
	          ]
	        },{
	          "featureType": "road",
	          "elementType": "labels.icon",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "road",
	          "elementType": "labels.text.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "road",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#606060" }
	          ]
	        },{
	          "featureType": "administrative",
	          "elementType": "labels.icon",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "administrative",
	          "elementType": "labels.text.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "administrative.country",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#6e6e6e" }
	          ]
	        },{
	          "featureType": "administrative.neighborhood",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#686868" }
	          ]
	        },{
	          "featureType": "transit.line",
	          "elementType": "geometry",
	          "stylers": [
	            { "color": "#171717" }
	          ]
	        },{
	          "featureType": "transit",
	          "elementType": "labels",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "transit.station",
	          "elementType": "geometry",
	          "stylers": [
	            { "color": "#202020" }
	          ]
	        },{
	          "featureType": "water",
	          "elementType": "labels.icon",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "water",
	          "elementType": "labels.text.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "water",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#6e6e6e" }
	          ]
	        },{
	          "elementType": "labels.icon",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "elementType": "labels.text.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "landscape",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#606060" }
	          ]
	        }
	      ]
	},{
		'title': 'Default',
		'id': 'layer-two',
		'mapStyle': 'dark',
		'markerStyle': 'blue',
		'styleArray': []
	}
];

bootstrap.activeLayer = 'layer-one';

bootstrap.mapOptions = {
	zoom: 13,
	mapTypeId: google.maps.MapTypeId.ROADMAP,
	center: new google.maps.LatLng(43.652073, -79.382293),
	disableDefaultUI: true
};

bootstrap.markerStyles = {
	'red':{
		id: 'red',
		options: {
			icon: 'https://maps.google.com/mapfiles/kml/shapes/schools_maps.png'
		}
	},
	'blue': {
		id: 'blue',
		options: {
			icon: 'https://maps.google.com/mapfiles/kml/shapes/library_maps.png'
		}
	}
};