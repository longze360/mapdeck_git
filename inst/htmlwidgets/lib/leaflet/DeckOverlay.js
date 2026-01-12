/**
 * DeckOverlay - Leaflet integration for deck.gl
 * Based on @deck.gl-community/leaflet
 * 
 * This implementation provides a Leaflet Layer that wraps deck.gl,
 * allowing deck.gl layers to be synchronized with Leaflet basemap.
 */

// DeckOverlay class - Leaflet layer that wraps deck.gl
var DeckOverlay = L.Layer.extend({
  options: {
    views: null,
    layers: [],
    initialViewState: null,
    getTooltip: null,
    controller: false
  },

  initialize: function(options) {
    L.Util.setOptions(this, options);
    this._container = null;
    this._deck = null;
    this._animate = undefined;
  },

  onAdd: function(map) {
    var pane = this.getPane();
    if (!pane) {
      return this;
    }

    this._map = map;

    // Create container div for deck.gl canvas
    this._container = L.DomUtil.create('div');
    this._container.className = 'leaflet-layer';
    if (map._zoomAnimated) {
      L.DomUtil.addClass(this._container, 'leaflet-zoom-animated');
    }

    pane.appendChild(this._container);

    // Create deck instance
    this._createDeck();

    // Initial update
    this._update();

    return this;
  },

  onRemove: function(map) {
    if (!this._container || !this._deck) {
      return this;
    }

    L.DomUtil.remove(this._container);
    this._container = undefined;

    if (this._deck.finalize) {
      this._deck.finalize();
    }
    this._deck = undefined;

    return this;
  },

  getEvents: function() {
    return {
      viewreset: this._reset.bind(this),
      movestart: this._onMoveStart.bind(this),
      moveend: this._onMoveEnd.bind(this),
      zoomstart: this._onZoomStart.bind(this),
      zoom: this._onZoom.bind(this),
      zoomend: this._onZoomEnd.bind(this)
    };
  },

  setProps: function(props) {
    Object.assign(this.options, props);

    if (this._deck) {
      this._deck.setProps(props);
    }
  },

  pickObject: function(opts) {
    if (!this._deck) {
      return null;
    }
    return this._deck.pickObject ? this._deck.pickObject(opts) : null;
  },

  pickMultipleObjects: function(opts) {
    if (!this._deck) {
      return [];
    }
    return this._deck.pickMultipleObjects ? this._deck.pickMultipleObjects(opts) : [];
  },

  pickObjects: function(opts) {
    if (!this._deck) {
      return [];
    }
    return this._deck.pickObjects ? this._deck.pickObjects(opts) : [];
  },

  _createDeck: function() {
    var self = this;
    var viewState = this._getViewState();

    var deckOptions = Object.assign({}, this.options, {
      parent: this._container,
      controller: false,
      style: { zIndex: 'auto' },
      viewState: viewState,
      onViewStateChange: function(params) {
        // Let deck.gl update internally, but don't sync back to Leaflet
      }
    });

    // Create deck.gl instance
    if (typeof deck !== 'undefined') {
      this._deck = new deck.Deck(deckOptions);
    } else {
      console.warn('deck.gl not loaded');
    }
  },

  _getViewState: function() {
    var map = this._map;
    return {
      longitude: map.getCenter().lng,
      latitude: map.getCenter().lat,
      zoom: map.getZoom() - 1,
      pitch: 0,
      bearing: 0
    };
  },

  _update: function() {
    if (!this._container || !this._deck) {
      return;
    }
    if (this._map._animatingZoom) {
      return;
    }

    var size = this._map.getSize();
    this._container.style.width = size.x + 'px';
    this._container.style.height = size.y + 'px';

    // Invert map position
    var offset = this._map._getMapPanePos().multiplyBy(-1);
    L.DomUtil.setPosition(this._container, offset);

    // Update deck.gl view
    this._updateDeckView();
  },

  _updateDeckView: function() {
    if (!this._deck) {
      return;
    }

    var viewState = this._getViewState();
    this._deck.setProps({ viewState: viewState });

    if (this._deck.redraw) {
      this._deck.redraw();
    }
  },

  _reset: function() {
    this._updateTransform(this._map.getCenter(), this._map.getZoom());
    this._update();
  },

  _onMoveStart: function() {
    this._pauseAnimation();
  },

  _onMoveEnd: function() {
    this._update();
    this._unpauseAnimation();
  },

  _onZoomStart: function() {
    this._pauseAnimation();
  },

  _onZoom: function() {
    this._updateTransform(this._map.getCenter(), this._map.getZoom());
  },

  _onZoomEnd: function() {
    this._unpauseAnimation();
  },

  _pauseAnimation: function() {
    if (!this._deck) {
      return;
    }

    if (this._deck.props && this._deck.props._animate) {
      this._animate = this._deck.props._animate;
      this._deck.setProps({ _animate: false });
    }
  },

  _unpauseAnimation: function() {
    if (!this._deck) {
      return;
    }

    if (this._animate) {
      this._deck.setProps({ _animate: this._animate });
      this._animate = undefined;
    }
  },

  _updateTransform: function(center, zoom) {
    if (!this._container) {
      return;
    }

    var scale = this._map.getZoomScale(zoom, this._map.getZoom());
    var position = L.DomUtil.getPosition(this._container);
    var viewHalf = this._map.getSize().multiplyBy(0.5);
    var currentCenterPoint = this._map.project(this._map.getCenter(), zoom);
    var destCenterPoint = this._map.project(center, zoom);
    var centerOffset = destCenterPoint.subtract(currentCenterPoint);
    var topLeftOffset = viewHalf
      .multiplyBy(-scale)
      .add(position)
      .add(viewHalf)
      .subtract(centerOffset);

    if (L.Browser.any3d) {
Util.setTransform(this      L.Dom._container, topLeftOffset, scale);
    } else {
      L.DomUtil.setPosition(this._container, topLeftOffset);
    }
  }
});

// Factory function
function createDeckOverlay(options) {
  return new DeckOverlay(options);
}

// Export for module systems
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { DeckOverlay: DeckOverlay, createDeckOverlay: createDeckOverlay };
}
