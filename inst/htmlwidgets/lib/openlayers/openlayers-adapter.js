/**
 * OpenLayers Adapter for deck.gl Integration
 * 
 * This adapter integrates OpenLayers with deck.gl overlay functionality,
 * providing support for various OpenLayers sources and layer configurations.
 */

// OpenLayers adapter class
class OpenLayersMapAdapter {
  constructor(container, options) {
    this.container = container;
    this.options = options;
    this.map = null;
    this.deckOverlay = null;
    this.layers = new Map();
    this.view = null;
    
    this.init();
  }
  
  /**
   * Initialize the OpenLayers map instance
   */
  init() {
    // Load OpenLayers if not already loaded
    if (typeof ol === 'undefined') {
      this.loadOpenLayersAPI().then(() => {
        this.createMap();
      });
    } else {
      this.createMap();
    }
  }
  
  /**
   * Load OpenLayers API dynamically
   */
  loadOpenLayersAPI() {
    return new Promise((resolve, reject) => {
      // Load CSS first
      const cssLink = document.createElement('link');
      cssLink.rel = 'stylesheet';
      cssLink.href = 'https://cdn.jsdelivr.net/npm/ol@v7.5.2/ol.css';
      document.head.appendChild(cssLink);
      
      // Load JavaScript
      const script = document.createElement('script');
      script.src = 'https://cdn.jsdelivr.net/npm/ol@v7.5.2/dist/ol.js';
      script.onload = resolve;
      script.onerror = reject;
      document.head.appendChild(script);
    });
  }
  
  /**
   * Create the OpenLayers map instance
   */
  createMap() {
    // Create view
    this.view = new ol.View({
      center: ol.proj.fromLonLat(this.options.center || [0, 0]),
      zoom: this.options.zoom || 2,
      rotation: this.options.rotation || 0,
      projection: this.options.projection || 'EPSG:3857',
      extent: this.options.extent,
      minZoom: this.options.min_zoom || 0,
      maxZoom: this.options.max_zoom || 19,
      constrainResolution: this.options.constrain_resolution || false,
      smoothResolutionConstraint: this.options.smooth_resolution_constraint !== false,
      smoothExtentConstraint: this.options.smooth_extent_constraint !== false,
      enableRotation: this.options.enable_rotation !== false
    });
    
    // Create base layer
    const baseLayer = this.createBaseLayer();
    
    // Create interactions
    const interactions = this.createInteractions();
    
    // Create controls
    const controls = this.createControls();
    
    // OpenLayers map configuration
    const mapConfig = {
      target: this.container,
      layers: [baseLayer],
      view: this.view,
      interactions: interactions,
      controls: controls
    };
    
    // Create OpenLayers map
    this.map = new ol.Map(mapConfig);
    
    // Initialize deck.gl overlay
    this.initDeckOverlay();
    
    // Set up event handlers
    this.setupEventHandlers();
  }
  
  /**
   * Create base layer based on source configuration
   */
  createBaseLayer() {
    const sourceConfig = this.getSourceConfig(this.options.source || 'OSM');
    
    let source;
    
    switch (sourceConfig.type) {
      case 'XYZ':
        source = new ol.source.XYZ({
          url: sourceConfig.url,
          attributions: sourceConfig.attribution,
          maxZoom: sourceConfig.max_zoom
        });
        break;
        
      case 'OSM':
        source = new ol.source.OSM({
          attributions: sourceConfig.attribution
        });
        break;
        
      case 'BingMaps':
        source = new ol.source.BingMaps({
          key: sourceConfig.api_key || this.options.api_key,
          imagerySet: sourceConfig.imagerySet || 'Aerial',
          maxZoom: sourceConfig.max_zoom
        });
        break;
        
      case 'WMTS':
        source = new ol.source.WMTS({
          url: sourceConfig.url,
          layer: sourceConfig.layer,
          matrixSet: sourceConfig.matrixSet,
          format: sourceConfig.format,
          attributions: sourceConfig.attribution
        });
        break;
        
      case 'WMS':
        source = new ol.source.TileWMS({
          url: sourceConfig.url,
          params: sourceConfig.params,
          attributions: sourceConfig.attribution
        });
        break;
        
      case 'VectorTile':
        source = new ol.source.VectorTile({
          format: new ol.format.MVT(),
          url: sourceConfig.url,
          attributions: sourceConfig.attribution
        });
        break;
        
      default:
        // Default to OSM
        source = new ol.source.OSM();
    }
    
    return new ol.layer.Tile({
      source: source
    });
  }
  
  /**
   * Create map interactions
   */
  createInteractions() {
    const interactions = ol.interaction.defaults({
      keyboard: this.options.keyboard_pan !== false && this.options.keyboard_zoom !== false,
      mouseWheelZoom: this.options.mouse_wheel_zoom !== false,
      doubleClickZoom: this.options.double_click_zoom !== false,
      dragPan: this.options.drag_pan !== false,
      pinchRotate: this.options.pinch_rotate !== false,
      pinchZoom: this.options.pinch_zoom !== false,
      altShiftDragRotate: this.options.alt_shift_drag_rotate === true
    });
    
    return interactions;
  }
  
  /**
   * Create map controls
   */
  createControls() {
    const controls = ol.control.defaults({
      attribution: true,
      zoom: true,
      rotate: this.options.enable_rotation !== false
    });
    
    return controls;
  }
  
  /**
   * Get source configuration for a given source name
   */
  getSourceConfig(sourceName) {
    const configs = {
      'OSM': {
        type: 'OSM',
        attribution: '© OpenStreetMap contributors',
        max_zoom: 19
      },
      'CartoDB.Positron': {
        type: 'XYZ',
        url: 'https://{1-4}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
        attribution: '© OpenStreetMap contributors © CARTO',
        max_zoom: 19
      },
      'CartoDB.DarkMatter': {
        type: 'XYZ',
        url: 'https://{1-4}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png',
        attribution: '© OpenStreetMap contributors © CARTO',
        max_zoom: 19
      },
      'ESRI.WorldImagery': {
        type: 'XYZ',
        url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
        attribution: 'Tiles © Esri',
        max_zoom: 18
      },
      'Stamen.Terrain': {
        type: 'XYZ',
        url: 'https://stamen-tiles-{a-d}.a.ssl.fastly.net/terrain/{z}/{x}/{y}{r}.png',
        attribution: 'Map tiles by Stamen Design, CC BY 3.0 — Map data © OpenStreetMap contributors',
        max_zoom: 18
      },
      'Stamen.Watercolor': {
        type: 'XYZ',
        url: 'https://stamen-tiles-{a-d}.a.ssl.fastly.net/watercolor/{z}/{x}/{y}.jpg',
        attribution: 'Map tiles by Stamen Design, CC BY 3.0 — Map data © OpenStreetMap contributors',
        max_zoom: 18
      },
      'BingMaps.Aerial': {
        type: 'BingMaps',
        imagerySet: 'Aerial',
        attribution: '© Microsoft Corporation',
        max_zoom: 19
      },
      'OSM.Vector': {
        type: 'VectorTile',
        url: 'https://{a-c}.tile.openstreetmap.org/{z}/{x}/{y}.pbf',
        format: 'MVT',
        attribution: '© OpenStreetMap contributors',
        max_zoom: 14
      }
    };
    
    return configs[sourceName] || configs['OSM'];
  }
  
  /**
   * Initialize deck.gl overlay on OpenLayers map
   */
  initDeckOverlay() {
    // Create deck.gl overlay for OpenLayers
    this.createCustomDeckOverlay();
  }
  
  /**
   * Create custom deck.gl overlay for OpenLayers
   */
  createCustomDeckOverlay() {
    // Create a canvas overlay for deck.gl
    const canvas = document.createElement('canvas');
    canvas.style.position = 'absolute';
    canvas.style.top = '0';
    canvas.style.left = '0';
    canvas.style.pointerEvents = 'none';
    canvas.style.zIndex = '1000';
    
    // Add canvas to map container
    this.container.appendChild(canvas);
    
    // Get initial view state
    const center = ol.proj.toLonLat(this.view.getCenter());
    
    // Initialize deck.gl with custom canvas
    this.deck = new deck.Deck({
      canvas: canvas,
      width: this.container.clientWidth,
      height: this.container.clientHeight,
      initialViewState: {
        longitude: center[0],
        latitude: center[1],
        zoom: this.view.getZoom(),
        pitch: 0, // OpenLayers has limited pitch support
        bearing: this.view.getRotation() * 180 / Math.PI
      },
      controller: false, // OpenLayers map handles interaction
      layers: []
    });
    
    // Sync deck.gl view with OpenLayers map
    this.syncViewStates();
  }
  
  /**
   * Synchronize deck.gl view state with OpenLayers map
   */
  syncViewStates() {
    const updateDeckView = () => {
      const center = ol.proj.toLonLat(this.view.getCenter());
      const zoom = this.view.getZoom();
      const rotation = this.view.getRotation();
      
      this.deck.setProps({
        viewState: {
          longitude: center[0],
          latitude: center[1],
          zoom: zoom,
          pitch: 0, // OpenLayers doesn't support pitch like Mapbox
          bearing: rotation * 180 / Math.PI
        }
      });
    };
    
    // Listen to OpenLayers view events
    this.view.on('change:center', updateDeckView);
    this.view.on('change:zoom', updateDeckView);
    this.view.on('change:rotation', updateDeckView);
    
    // Handle resize
    window.addEventListener('resize', () => {
      this.deck.setProps({
        width: this.container.clientWidth,
        height: this.container.clientHeight
      });
    });
  }
  
  /**
   * Set up event handlers for map interactions
   */
  setupEventHandlers() {
    // Handle click events
    this.map.on('click', (e) => {
      const coordinate = ol.proj.toLonLat(e.coordinate);
      const pixel = e.pixel;
      
      this.handleMapClick({
        coordinate: coordinate,
        pixel: pixel
      });
    });
    
    // Handle view state changes
    this.view.on('change', () => {
      this.handleViewStateChange();
    });
    
    // Handle pointer move for hover effects
    this.map.on('pointermove', (e) => {
      const coordinate = ol.proj.toLonLat(e.coordinate);
      const pixel = e.pixel;
      
      this.handleMapHover({
        coordinate: coordinate,
        pixel: pixel
      });
    });
  }
  
  /**
   * Handle map click events
   */
  handleMapClick(event) {
    // Dispatch click event to R
    if (window.Shiny) {
      Shiny.setInputValue(this.container.id + '_click', {
        coordinate: event.coordinate,
        pixel: event.pixel,
        timestamp: Date.now()
      });
    }
  }
  
  /**
   * Handle map hover events
   */
  handleMapHover(event) {
    // Dispatch hover event to R
    if (window.Shiny) {
      Shiny.setInputValue(this.container.id + '_hover', {
        coordinate: event.coordinate,
        pixel: event.pixel,
        timestamp: Date.now()
      });
    }
  }
  
  /**
   * Handle view state changes
   */
  handleViewStateChange() {
    const center = ol.proj.toLonLat(this.view.getCenter());
    const zoom = this.view.getZoom();
    const rotation = this.view.getRotation();
    
    // Dispatch view state to R
    if (window.Shiny) {
      Shiny.setInputValue(this.container.id + '_view_state', {
        longitude: center[0],
        latitude: center[1],
        zoom: zoom,
        pitch: 0, // OpenLayers doesn't support pitch
        bearing: rotation * 180 / Math.PI,
        timestamp: Date.now()
      });
    }
  }
  
  /**
   * Add deck.gl layer
   */
  addLayer(layerConfig) {
    // Create deck.gl layer
    const layer = this.createDeckLayer(layerConfig);
    
    // Add to deck.gl
    const currentLayers = this.deck.props.layers || [];
    this.deck.setProps({
      layers: [...currentLayers, layer]
    });
    
    // Store layer reference
    this.layers.set(layerConfig.id, layer);
  }
  
  /**
   * Remove deck.gl layer
   */
  removeLayer(layerId) {
    if (this.layers.has(layerId)) {
      const currentLayers = this.deck.props.layers || [];
      const filteredLayers = currentLayers.filter(layer => layer.id !== layerId);
      
      this.deck.setProps({
        layers: filteredLayers
      });
      
      this.layers.delete(layerId);
    }
  }
  
  /**
   * Create deck.gl layer from configuration
   */
  createDeckLayer(config) {
    // This would create the appropriate deck.gl layer based on config.type
    // For now, return a basic configuration
    return {
      id: config.id,
      type: config.type,
      data: config.data,
      ...config.props
    };
  }
  
  /**
   * Update map source
   */
  updateSource(source) {
    // Get new source configuration
    const sourceConfig = this.getSourceConfig(source);
    
    // Remove existing base layer
    const layers = this.map.getLayers();
    if (layers.getLength() > 0) {
      layers.removeAt(0);
    }
    
    // Create new base layer
    const newBaseLayer = this.createBaseLayer();
    layers.insertAt(0, newBaseLayer);
  }
  
  /**
   * Set map view
   */
  setView(longitude, latitude, zoom, pitch, bearing) {
    const center = ol.proj.fromLonLat([longitude, latitude]);
    
    this.view.setCenter(center);
    if (zoom !== undefined) this.view.setZoom(zoom);
    if (bearing !== undefined) this.view.setRotation(bearing * Math.PI / 180);
    
    // Note: OpenLayers doesn't support pitch like Mapbox
    if (pitch !== undefined && pitch !== 0) {
      console.warn('OpenLayers does not support pitch - ignoring pitch parameter');
    }
  }
  
  /**
   * Get current view state
   */
  getViewState() {
    const center = ol.proj.toLonLat(this.view.getCenter());
    const zoom = this.view.getZoom();
    const rotation = this.view.getRotation();
    
    return {
      longitude: center[0],
      latitude: center[1],
      zoom: zoom,
      pitch: 0,
      bearing: rotation * 180 / Math.PI
    };
  }
  
  /**
   * Resize map
   */
  resize() {
    this.map.updateSize();
    
    if (this.deck) {
      this.deck.setProps({
        width: this.container.clientWidth,
        height: this.container.clientHeight
      });
    }
  }
  
  /**
   * Destroy the map instance
   */
  destroy() {
    if (this.deck) {
      this.deck.finalize();
    }
    
    if (this.map) {
      this.map.setTarget(null);
    }
    
    this.layers.clear();
  }
}

// Export for use in mapdeck
window.OpenLayersMapAdapter = OpenLayersMapAdapter;