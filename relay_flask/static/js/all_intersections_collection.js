var AllIntersectionsCollection = Backbone.Collection.extend({

	// define the collection type
	model: IntersectionModel,

	// fetch()
	// temporary replacement/override for getting the intersections.
	// backend should be RESTful or override backbone.sync for a socket connection
	fetch: function(){
		var bounds = new Object();
		bounds.minlat = -90;
		bounds.minlong = -180;
		bounds.maxlat = 90;
		bounds.maxlong = 180;

		var data = null;

		$.ajax({
			type: "POST",
			datatype: "JSON",
			contentType: "application/json",
			url: "http://localhost:5000/request_all_intersections",
			data: JSON.stringify(bounds),
			async: false
        }).then( function(d){
        	data = d;
        	s.restConnected();
        	s.globalUpdate();
        });

        this.add(data); // adds all the intersection objects to the collection
	},

	// update: function(){
	// 	var bounds = new Object();
	// 	bounds.minlat = -90;
	// 	bounds.minlong = -180;
	// 	bounds.maxlat = 90;
	// 	bounds.maxlong = 180;

	// 	var data = null;

	// 	$.ajax({
	// 		type: "POST",
	// 		datatype: "JSON",
	// 		contentType: "application/json",
	// 		url: "http://localhost:5000/request_intersections",
	// 		data: JSON.stringify(bounds),
	// 		async: false
 //        }).then( function(d){
 //        	data = d;
 //        	s.restConnected();
 //        	s.globalUpdate();
 //        });

 //        this.set(data); // adds all the intersection objects to the collection
	// }

});