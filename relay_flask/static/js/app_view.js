var AppView = Backbone.View.extend({

	// HTML object associated with AppView
	el: $('#app'),

	// Other references:
	header: $('#header'),
	activeTab: $('#map-header-btn'),
	mapHeaderBtn: $('#map-header-btn'),
	intersectionsHeaderBtn: $('#intersections-header-btn'),
	roadsHeaderBtn: $('#roads-header-btn'),
	networkHeaderbtn: $('#network-header-btn'),

	activePage: 'mapPageView',

	// Events
	events: {
		'click .page-btn' : 'pageSelected',
	},

	// initialize()
	// starts the app
	initialize: function() {

		// Get data
		// the app view is responsible for collecting the data and keeping it up to date.
		// the pages read from here and build their own views and collectionviews from here.
		this.intersectionsCollection = new IntersectionsCollection();
		this.allIntersectionsCollection = new AllIntersectionsCollection();
		this.roadsCollection = new RoadsCollection();

		this.intersectionsCollection.fetch();
		this.allIntersectionsCollection.fetch();
		this.roadsCollection.fetch();

		// Get pages
		this.mapPageView = new MapPageView({ic: this.intersectionsCollection, aic: this.allIntersectionsCollection, rc: this.roadsCollection});
		this.intersectionsPageView = new IntersectionsPageView({ic: this.intersectionsCollection, rc: this.roadsCollection});
		this.roadsPageView = new RoadsPageView({ic: this.intersectionsCollection, rc: this.roadsCollection});
		this.networkPageView = new NetworkPageView({ic: this.intersectionsCollection, rc: this.roadsCollection});

		this.render();
	},

	render: function(){
		this.setPage(this.mapHeaderBtn);
	},

	pageSelected: function(e){
		this.setPage($('#' + e.target.id));
	},

	setPage: function(pageTab){
		this.activeTab.removeClass('active'); // unselect the active tab
		$( '#' + this.activeTab.data('page-id')).removeClass('page-active'); // hide the active tab

		this.activeTab = pageTab;

		this.activeTab.addClass('active'); // select the new tab
		$( '#' + this.activeTab.data('page-id')).addClass('page-active'); // show the new tab

		this[this.activePage].setInactive();
		var newPage = this.activeTab.data('page-name');
		this[newPage].setActive();
		this.activePage = newPage;
	},

	setActive: function(){

	},

	setInactive: function(){

	}
});