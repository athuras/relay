var StatusPaneView = Backbone.View.extend({
	el: $('#status-container'),

	events: {
		'click #status': 'togglePane',
	},

	intervalLength: 1000,

	initialize: function(){
		// default values
		this.restOn = true;
		this.socketOn = false;
		this.restOk = false;
		this.socketOk = false;
		this.rMessage = '-';
		this.sMessage = '-';
		this.lastLocalUpdate = Date.now();
		this.lastGlobalUpdate = Date.now();

		// listener
		$(document).keyup($.proxy(function(e){
			this.escapeKeyed(e);
		}, this));

		this.render();
		this.startInterval();
	},

	render: function(){
		// write the connection statuses
		if(this.restOn){
			if(this.restOk){
				this.rMessage = 'OK';
			} else {
				this.rMessage = 'CONNECTING';
			}
		} else{
			this.rMessage = 'OFF';
		}
		if(this.socketOn){
			if(this.socketOk){
				this.sMessage = 'OK';
			} else {
				this.sMessage = 'CONNECTING';
			}
		} else{
			this.sMessage = 'OFF';
		}

		this.$('#rest-status').html(this.rMessage);
		this.$('#socket-status').html(this.sMessage);

		// write the connection times
		this.$('#local-update').html(Math.floor((Date.now() - this.lastLocalUpdate) / 1000) + 's ago.');
		this.$('#global-update').html(Math.floor((Date.now() - this.lastGlobalUpdate) / 1000) + 's ago.');

	},


	escapeKeyed: function(e){
		if(this.$('#status-pane').is(':visible')){
			this.togglePane();
		}
	},

	togglePane: function(){
		this.$('#status-pane').toggle();
	},

	// successfully connected via rest
	restConnected: function(){
		this.restOn = true;
		this.restOk = true;
	},

	localUpdate: function(){
		this.lastLocalUpdate = Date.now();
	},

	globalUpdate: function(){
		this.lastGlobalUpdate = Date.now();
		this.lastLocalUpdate = Date.now();
	},

	startInterval: function(){
		this.interval = setInterval(function(){
			s.render();
		}, this.intervalLength);
	},

	stopInterval: function(){
		stopInterval(this.interval);
	}

});