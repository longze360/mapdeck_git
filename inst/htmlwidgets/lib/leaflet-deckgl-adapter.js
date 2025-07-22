/**
 * Leaflet-deck.gl Adapter
 * 
 * This adapter integrates Leaflet.js with deck.gl for the mapdeck package.
 * It provides a bridge between Leaflet's tile-based mapping and deck.gl's
 * WebGL-based data visualization layers.
 */

// Global namespace for Leaflet adapter
window.MapdeckLeafletAdapter = (function() {
  'use strict';

  // Store active Leaflet maps
  const leafletMaps = new Map();
  
  // Store deck.gl overlays
  const deckOverlays = new Map();

  /**
   * Initialize Leaflet map with deck.gl overlay
   */
  function initializeLeafletMap(mapId, options) {
    try {
      // Create Leaflet map
      const map = L.map(mapId, {
        center: options.center || [0, 0],
        zoom: options.zoom || 10,
        minZoom: options.min_zoom || 1,
        maxZoom: options.max_zoom || 18,
        zoomControl: options.zoom_control !== false,
        attributionControl: options.attribution_control !== false,
        worldCopyJump: options.world_copy_jump || false,
        maxBounds: options.max_bounds || null,
        detectRetina: options.detect_retina !== false
      });

      // Add tile layer
      const tileLayer = createTileLayer(options);
      tileLayer.addTo(map);

      // Create deck.gl overlay
      const deckOverlay = createDeckGLOverlay(map, options);
      
      // Store references
      leafletMaps.set(mapId, map);
      deckOverlays.set(mapId, deckOverlay);

      // Set up event handlers
      setupEventHandlers(map, deckOverlay, mapId);

      return {
        leafletMap: map,
        deckOverlay: deckOverlay
      };
    } catch (error) {
      console.error('Failed to initialize Leaflet map:', error);
      throw error;
    }
  }

  /**
   * Create appropriate tile layer based on provider
   */
  function createTileLayer(options) {
    const provider = options.tile_provider || 'OpenStreetMap';
    
    // Custom tile URL
    if (options.tile_url) {
      return L.tileLayer(options.tile_url, {
        attribution: options.attribution || '',
        maxZoom: options.max_zoom || 18,
        minZoom: options.min_zoom || 1,
        detectRetina: options.detect_retina !== false
      });
    }

    // Predefined tile providers
    const tileProviders = {
      'OpenStreetMap': {
        url: 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
        attribution: '© OpenStreetMap contributors'
      },
      'OpenStreetMap.Mapnik': {
        url: 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
        attribution: '© OpenStreetMap contributors'
      },
      'OpenStreetMap.BlackAndWhite': {
        url: 'https://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png',
        attribution: '© OpenStreetMap contributors'
      },
      'CartoDB.Positron': {
        url: 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
        attribution: '© OpenStreetMap contributors © CARTO'
      },
      'CartoDB.PositronNoLabels': {
        url: 'https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png',
        attribution: '© OpenStreetMap contributors © CARTO'
      },
      'CartoDB.DarkMatter': {
        url: 'https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png',
        attribution: '© OpenStreetMap contributors © CARTO'
      },
      'CartoDB.DarkMatterNoLabels': {
        url: 'https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png',
        attribution: '© OpenStreetMap contributors © CARTO'
      },
      'Esri.WorldImagery': {
        url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
        attribution: 'Tiles © Esri'
      },
      'Esri.WorldTopoMap': {
        url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',
        attribution: 'Tiles © Esri'
      },
      'OpenTopoMap': {
        url: 'https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png',
        attribution: '© OpenStreetMap contributors, © OpenTopoMap'
      },
      'Stamen.Toner': {
        url: 'https://stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}{r}.png',
        attribution: 'Map tiles by Stamen Design, CC BY 3.0'
      },
      'Stamen.TonerBackground': {
        url: 'https://stamen-tiles-{s}.a.ssl.fastly.net/toner-background/{z}/{x}/{y}{r}.png',
        attribution: 'Map tiles by Stamen Design, CC BY 3.0'
      },
      'Stamen.Watercolor': {
        url: 'https://stamen-tiles-{s}.a.ssl.fastly.net/watercolor/{z}/{x}/{y}.jpg',
        attribution: 'Map tiles by Stamen Design, CC BY 3.0'
      }
    };

    const tileConfig = tileProviders[provider] || tileProviders['OpenStreetMap'];
    
    return L.tileLayer(tileConfig.url, {
      attribution: options.attribution || tileConfig.attribution,
      maxZoom: options.max_zoom || 18,
      minZoom: options.min_zoom || 1,
      detectRetina: options.detect_retina !== false
    });
  }

  /**
   * Create deck.gl overlay for Leaflet map
   */
  function createDeckGLOverlay(leafletMap, options) {
    // Create deck.gl overlay that syncs with Leaflet
    const deckOverlay = new deck.DeckGL({
      container: leafletMap.getContainer(),
      initialViewState: {
        longitude: leafletMap.getCenter().lng,
        latitude: leafletMap.getCenter().lat,
        zoom: leafletMap.getZoom(),
        pitch: 0, // Leaflet doesn't support pitch
        bearing: 0 // Leaflet doesn't support bearing
      },
      controller: false, // Let Leaflet handle interactions
      layers: [],
      // Sync with Leaflet's coordinate system
      coordinateSystem: deck.COORDINATE_SYSTEM.LNGLAT,
      // Use Leaflet's projection
      getTooltip: options.getTooltip || null
    });

    // Position deck.gl overlay to match Leaflet
    const container = deckOverlay.canvas;
    container.style.position = 'absolute';
    container.style.top = '0';
    container.style.left = '0';
    container.style.pointerEvents = 'none'; // Let Leaflet handle interactions
    container.style.zIndex = '200'; // Above Leaflet tiles but below controls

    return deckOverlay;
  }

  /**
   * Set up event handlers to sync Leaflet and deck.gl
   */
  function setupEventHandlers(leafletMap, deckOverlay, mapId) {
    // Sync deck.gl view with Leaflet view changes
    function syncDeckGLView() {
      const center = leafletMap.getCenter();
      const zoom = leafletMap.getZoom();
      
      deckOverlay.setProps({
        viewState: {
          longitude: center.lng,
          latitude: center.lat,
          zoom: zoom,
          pitch: 0,
          bearing: 0
        }
      });
    }

    // Listen to Leaflet map events
    leafletMap.on('move', syncDeckGLView);
    leafletMap.on('zoom', syncDeckGLView);
    leafletMap.on('resize', function() {
      deckOverlay.setProps({
        width: leafletMap.getContainer().clientWidth,
        height: leafletMap.getContainer().clientHeight
      });
    });

    // Initial sync
    syncDeckGLView();
  }

  /**
   * Update tile provider for existing map
   */
  function updateTileProvider(mapId, newProvider) {
    const map = leafletMaps.get(mapId);
    if (!map) {
      console.error('Map not found:', mapId);
      return;
    }

    // Remove existing tile layers
    map.eachLayer(function(layer) {
      if (layer instanceof L.TileLayer) {
        map.removeLayer(layer);
      }
    });

    // Add new tile layer
    const tileLayer = createTileLayer({ tile_provider: newProvider });
    tileLayer.addTo(map);
  }

  /**
   * Add deck.gl layer to overlay
   */
  function addDeckGLLayer(mapId, layer) {
    const deckOverlay = deckOverlays.get(mapId);
    if (!deckOverlay) {
      console.error('Deck overlay not found for map:', mapId);
      return;
    }

    const currentLayers = deckOverlay.props.layers || [];
    deckOverlay.setProps({
      layers: [...currentLayers, layer]
    });
  }

  /**
   * Remove deck.gl layer from overlay
   */
  function removeDeckGLLayer(mapId, layerId) {
    const deckOverlay = deckOverlays.get(mapId);
    if (!deckOverlay) {
      console.error('Deck overlay not found for map:', mapId);
      return;
    }

    const currentLayers = deckOverlay.props.layers || [];
    const filteredLayers = currentLayers.filter(layer => layer.id !== layerId);
    
    deckOverlay.setProps({
      layers: filteredLayers
    });
  }

  /**
   * Clear all deck.gl layers
   */
  function clearDeckGLLayers(mapId) {
    const deckOverlay = deckOverlays.get(mapId);
    if (!deckOverlay) {
      console.error('Deck overlay not found for map:', mapId);
      return;
    }

    deckOverlay.setProps({
      layers: []
    });
  }

  /**
   * Set map view
   */
  function setView(mapId, longitude, latitude, zoom) {
    const map = leafletMaps.get(mapId);
    if (!map) {
      console.error('Map not found:', mapId);
      return;
    }

    map.setView([latitude, longitude], zoom);
  }

  /**
   * Get map instance
   */
  function getMap(mapId) {
    return leafletMaps.get(mapId);
  }

  /**
   * Get deck overlay instance
   */
  function getDeckOverlay(mapId) {
    return deckOverlays.get(mapId);
  }

  /**
   * Destroy map and clean up resources
   */
  function destroyMap(mapId) {
    const map = leafletMaps.get(mapId);
    const deckOverlay = deckOverlays.get(mapId);

    if (deckOverlay) {
      deckOverlay.finalize();
      deckOverlays.delete(mapId);
    }

    if (map) {
      map.remove();
      leafletMaps.delete(mapId);
    }
  }

  // Public API
  return {
    initializeLeafletMap: initializeLeafletMap,
    updateTileProvider: updateTileProvider,
    addDeckGLLayer: addDeckGLLayer,
    removeDeckGLLayer: removeDeckGLLayer,
    clearDeckGLLayers: clearDeckGLLayers,
    setView: setView,
    getMap: getMap,
    getDeckOverlay: getDeckOverlay,
    destroyMap: destroyMap
  };
})();

// Make adapter available globally for mapdeck integration
if (typeof window !== 'undefined') {
  window.MapdeckLeafletAdapter = window.MapdeckLeafletAdapter;
}