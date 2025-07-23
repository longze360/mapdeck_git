/**
 * Provider Dependencies Manager
 * 
 * This file manages the loading of JavaScript dependencies for different map providers
 * in the mapdeck package. It ensures that the appropriate libraries and adapters
 * are loaded based on the selected provider.
 */

// Global namespace for provider dependencies
window.MapdeckProviderDependencies = (function() {
  'use strict';

  // Track loaded providers
  const loadedProviders = new Set();
  
  // Default provider
  const DEFAULT_PROVIDER = 'mapbox';
  
  /**
   * Load provider-specific dependencies
   * 
   * @param {string} provider - The provider name ('mapbox', 'leaflet', 'openlayers', 'gaode', 'baidu')
   * @param {Object} options - Provider-specific options
   * @returns {Promise} - Promise that resolves when dependencies are loaded
   */
  function loadProviderDependencies(provider, options = {}) {
    // Default to Mapbox if no provider specified
    provider = provider || DEFAULT_PROVIDER;
    
    // If already loaded, return resolved promise
    if (loadedProviders.has(provider)) {
      return Promise.resolve();
    }
    
    // Load provider-specific dependencies
    switch (provider) {
      case 'mapbox':
        return loadMapboxDependencies(options);
      case 'leaflet':
        return loadLeafletDependencies(options);
      case 'openlayers':
        return loadOpenLayersDependencies(options);
      case 'gaode':
        return loadGaodeDependencies(options);
      case 'baidu':
        return loadBaiduDependencies(options);
      default:
        console.warn(`Unknown provider: ${provider}, falling back to Mapbox`);
        return loadMapboxDependencies(options);
    }
  }
  
  /**
   * Load Mapbox dependencies
   */
  function loadMapboxDependencies(options) {
    // Mapbox dependencies are loaded by default
    loadedProviders.add('mapbox');
    return Promise.resolve();
  }
  
  /**
   * Load Leaflet dependencies
   */
  function loadLeafletDependencies(options) {
    return new Promise((resolve, reject) => {
      // Check if Leaflet is already loaded
      if (typeof L !== 'undefined' && typeof window.MapdeckLeafletAdapter !== 'undefined') {
        loadedProviders.add('leaflet');
        resolve();
        return;
      }
      
      // Load Leaflet CSS
      const cssLink = document.createElement('link');
      cssLink.rel = 'stylesheet';
      cssLink.href = 'https://unpkg.com/leaflet@1.9.4/dist/leaflet.css';
      document.head.appendChild(cssLink);
      
      // Load Leaflet JS
      const script = document.createElement('script');
      script.src = 'https://unpkg.com/leaflet@1.9.4/dist/leaflet.js';
      
      script.onload = function() {
        // Load Leaflet-deck.gl adapter
        const adapterScript = document.createElement('script');
        adapterScript.src = `${getLibraryPath()}/leaflet-deckgl-adapter.js`;
        
        adapterScript.onload = function() {
          loadedProviders.add('leaflet');
          resolve();
        };
        
        adapterScript.onerror = function() {
          reject(new Error('Failed to load Leaflet-deck.gl adapter'));
        };
        
        document.head.appendChild(adapterScript);
      };
      
      script.onerror = function() {
        reject(new Error('Failed to load Leaflet'));
      };
      
      document.head.appendChild(script);
    });
  }
  
  /**
   * Load OpenLayers dependencies
   */
  function loadOpenLayersDependencies(options) {
    return new Promise((resolve, reject) => {
      // Check if OpenLayers is already loaded
      if (typeof ol !== 'undefined' && typeof window.OpenLayersMapAdapter !== 'undefined') {
        loadedProviders.add('openlayers');
        resolve();
        return;
      }
      
      // Load OpenLayers CSS
      const cssLink = document.createElement('link');
      cssLink.rel = 'stylesheet';
      cssLink.href = `${getLibraryPath()}/openlayers/ol.css`;
      document.head.appendChild(cssLink);
      
      // Load OpenLayers JS
      const script = document.createElement('script');
      script.src = `${getLibraryPath()}/openlayers/ol.js`;
      
      script.onload = function() {
        // Load OpenLayers-deck.gl adapter
        const adapterScript = document.createElement('script');
        adapterScript.src = `${getLibraryPath()}/openlayers/openlayers-adapter.js`;
        
        adapterScript.onload = function() {
          loadedProviders.add('openlayers');
          resolve();
        };
        
        adapterScript.onerror = function() {
          reject(new Error('Failed to load OpenLayers-deck.gl adapter'));
        };
        
        document.head.appendChild(adapterScript);
      };
      
      script.onerror = function() {
        reject(new Error('Failed to load OpenLayers'));
      };
      
      document.head.appendChild(script);
    });
  }
  
  /**
   * Load Gaode Maps dependencies
   */
  function loadGaodeDependencies(options) {
    return new Promise((resolve, reject) => {
      // Check if Gaode adapter is already loaded
      if (typeof window.GaodeMapAdapter !== 'undefined') {
        loadedProviders.add('gaode');
        resolve();
        return;
      }
      
      // Load Gaode adapter
      const adapterScript = document.createElement('script');
      adapterScript.src = `${getLibraryPath()}/gaode/gaode-adapter.js`;
      
      adapterScript.onload = function() {
        loadedProviders.add('gaode');
        resolve();
      };
      
      adapterScript.onerror = function() {
        reject(new Error('Failed to load Gaode Maps adapter'));
      };
      
      document.head.appendChild(adapterScript);
    });
  }
  
  /**
   * Load Baidu Maps dependencies
   */
  function loadBaiduDependencies(options) {
    return new Promise((resolve, reject) => {
      // Check if Baidu adapter is already loaded
      if (typeof window.BaiduMapAdapter !== 'undefined') {
        loadedProviders.add('baidu');
        resolve();
        return;
      }
      
      // Load Baidu adapter
      const adapterScript = document.createElement('script');
      adapterScript.src = `${getLibraryPath()}/baidu/baidu-adapter.js`;
      
      adapterScript.onload = function() {
        loadedProviders.add('baidu');
        resolve();
      };
      
      adapterScript.onerror = function() {
        reject(new Error('Failed to load Baidu Maps adapter'));
      };
      
      document.head.appendChild(adapterScript);
    });
  }
  
  /**
   * Get the path to the library files
   */
  function getLibraryPath() {
    // Try to find the script path
    const scripts = document.getElementsByTagName('script');
    for (let i = 0; i < scripts.length; i++) {
      const src = scripts[i].src;
      if (src.indexOf('mapdeck.js') !== -1) {
        return src.replace(/mapdeck\.js$/, 'lib');
      }
    }
    
    // Fallback to a relative path
    return 'lib';
  }
  
  /**
   * Initialize a provider
   */
  function initializeProvider(mapId, provider, options) {
    return loadProviderDependencies(provider, options)
      .then(() => {
        // Create provider instance based on type
        switch (provider) {
          case 'leaflet':
            return window.MapdeckLeafletAdapter.initializeLeafletMap(mapId, options);
          case 'openlayers':
            return new window.OpenLayersMapAdapter(mapId, options);
          case 'gaode':
            return new window.GaodeMapAdapter(mapId, options);
          case 'baidu':
            return new window.BaiduMapAdapter(mapId, options);
          default:
            throw new Error(`Provider ${provider} not supported or dependencies not loaded`);
        }
      });
  }
  
  /**
   * Check if a provider is loaded
   */
  function isProviderLoaded(provider) {
    return loadedProviders.has(provider);
  }
  
  // Public API
  return {
    loadProviderDependencies: loadProviderDependencies,
    initializeProvider: initializeProvider,
    isProviderLoaded: isProviderLoaded
  };
})();

// Make dependencies manager available globally
if (typeof window !== 'undefined') {
  window.MapdeckProviderDependencies = window.MapdeckProviderDependencies;
}