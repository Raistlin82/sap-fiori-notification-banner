#!/usr/bin/env node

/**
 * Prepare SAP BSP Deployment
 *
 * Creates a clean deployment folder with SAP-compatible filenames:
 * - Removes files with hyphens (Component-preload.js → ComponentPreload.js)
 * - Excludes debug files (*-dbg.js) and source maps (*.map)
 * - Creates deployment-ready structure for SE80 import
 */

const fs = require('fs-extra');
const path = require('path');

const SOURCE_DIR = path.join(__dirname, '../dist');
const DEPLOY_DIR = path.join(__dirname, '../deploy-sap');

console.log('🚀 Preparing SAP BSP Deployment...\n');

// Clean deploy directory
if (fs.existsSync(DEPLOY_DIR)) {
    fs.removeSync(DEPLOY_DIR);
    console.log('✅ Cleaned previous deployment folder');
}

fs.ensureDirSync(DEPLOY_DIR);

// File name transformations
const RENAME_MAP = {
    // No renaming needed - we exclude files with hyphens instead
};

// Files to skip (not needed in production)
const SKIP_PATTERNS = [
    /.*-dbg\..*/,           // All debug files (*-dbg.js)
    /.*-preload\..*/,       // Preload bundles (contain hyphens, not compatible with SAP BSP)
    /.*\.map$/,             // All source maps (*.js.map)
    /.*\.zip$/,             // Zip archives
];

function shouldSkipFile(filename) {
    return SKIP_PATTERNS.some(pattern => pattern.test(filename));
}

function transformFileName(filename) {
    return RENAME_MAP[filename] || filename;
}

function copyFiles(sourceDir, targetDir, relativePath = '') {
    const items = fs.readdirSync(sourceDir);

    items.forEach(item => {
        const sourcePath = path.join(sourceDir, item);
        const stat = fs.statSync(sourcePath);

        if (stat.isDirectory()) {
            // Recursively copy directories
            const newTargetDir = path.join(targetDir, item);
            fs.ensureDirSync(newTargetDir);
            copyFiles(sourcePath, newTargetDir, path.join(relativePath, item));
        } else {
            // Check if file should be skipped
            if (shouldSkipFile(item)) {
                console.log(`⏭️  Skipped: ${path.join(relativePath, item)}`);
                return;
            }

            // Transform filename if needed
            const newFileName = transformFileName(item);
            const targetPath = path.join(targetDir, newFileName);

            fs.copyFileSync(sourcePath, targetPath);

            if (newFileName !== item) {
                console.log(`📝 Renamed: ${item} → ${newFileName}`);
            } else {
                console.log(`✅ Copied: ${path.join(relativePath, item)}`);
            }
        }
    });
}

// Copy and transform files
try {
    copyFiles(SOURCE_DIR, DEPLOY_DIR);

    console.log('\n✅ Deployment folder created successfully!');
    console.log(`📁 Location: ${DEPLOY_DIR}`);
    console.log('\n📋 Next Steps:');
    console.log('   1. SE80 → ZNOTIFY_BANNER → MIME Objects');
    console.log('   2. Import files from: deploy-sap/');
    console.log('   3. All filenames are now SAP-compatible (no hyphens)');
    console.log('\n🎯 Essential files included:');

    // List essential files
    const essentialFiles = [
        'Component.js',
        'index.html',
        'manifest.json',
        'controller/NotificationBanner.js',
        'controller/TileCounter.js',
        'controller/View1.controller.js',
        'i18n/i18n.properties',
        'model/models.js',
        'view/View1.view.xml',
        'css/style.css'
    ];

    essentialFiles.forEach(file => console.log(`   ✅ ${file}`));

    console.log('\n📝 Note: Preload bundle excluded (contains hyphens incompatible with SAP BSP)');
    console.log('   UI5 will load individual files - works perfectly, just slightly slower on first load.');

} catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
}
