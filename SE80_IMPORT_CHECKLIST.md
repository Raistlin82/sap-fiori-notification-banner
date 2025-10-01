# SE80 MIME Objects Import - Quick Checklist

**BSP Application**: ZNOTIFY_BANNER
**Source Folder**: `dist/`
**Total Files**: 23

---

## ✅ Import Checklist

### Step 1: Root Files (8 files)
- [ ] Component.js
- [ ] Component-dbg.js
- [ ] ⭐ Component-preload.js (CRITICAL - minified bundle)
- [ ] Component-preload.js.map
- [ ] Component.js.map
- [ ] ⭐ index.html (CRITICAL - entry point)
- [ ] ⭐ manifest.json (CRITICAL - app descriptor)
- [ ] sap_fiori_notification_banner.zip (OPTIONAL)

**Action**: Right-click "MIME Objects" → Import → Browse to `dist/` → Select 8 files (Ctrl+Click) → Import

---

### Step 2: controller/ Folder (9 files)
- [ ] Create folder: "controller"
- [ ] ⭐ NotificationBanner.js (CRITICAL)
- [ ] NotificationBanner-dbg.js
- [ ] NotificationBanner.js.map
- [ ] ⭐ TileCounter.js (CRITICAL)
- [ ] TileCounter-dbg.js
- [ ] TileCounter.js.map
- [ ] View1.controller.js
- [ ] View1-dbg.controller.js
- [ ] View1.controller.js.map

**Action**:
1. Right-click "MIME Objects" → Create → Folder → Name: "controller"
2. Right-click "controller" → Import → Browse to `dist/controller/` → Select 9 files → Import

---

### Step 3: css/ Folder (1 file)
- [ ] Create folder: "css"
- [ ] style.css

**Action**:
1. Right-click "MIME Objects" → Create → Folder → Name: "css"
2. Right-click "css" → Import → Browse to `dist/css/` → Select style.css → Import

---

### Step 4: i18n/ Folder (1 file)
- [ ] Create folder: "i18n"
- [ ] ⭐ i18n.properties (CRITICAL - translations)

**Action**:
1. Right-click "MIME Objects" → Create → Folder → Name: "i18n"
2. Right-click "i18n" → Import → Browse to `dist/i18n/` → Select i18n.properties → Import

---

### Step 5: model/ Folder (3 files)
- [ ] Create folder: "model"
- [ ] models.js
- [ ] models-dbg.js
- [ ] models.js.map

**Action**:
1. Right-click "MIME Objects" → Create → Folder → Name: "model"
2. Right-click "model" → Import → Browse to `dist/model/` → Select 3 files → Import

---

### Step 6: view/ Folder (1 file)
- [ ] Create folder: "view"
- [ ] View1.view.xml

**Action**:
1. Right-click "MIME Objects" → Create → Folder → Name: "view"
2. Right-click "view" → Import → Browse to `dist/view/` → Select View1.view.xml → Import

---

### Step 7: Activate All
- [ ] Right-click on ZNOTIFY_BANNER root → Mass Activate → Object List
- [ ] Select all MIME objects → Activate
- [ ] All files show green traffic light ✅

---

## 🎯 Critical Files (Must Import)

These 6 files are ESSENTIAL for the application to work:

1. **Component-preload.js** - Minified bundle with all UI5 components
2. **index.html** - Application entry point (loaded by Fiori Launchpad)
3. **manifest.json** - App descriptor (datasources, routing, i18n)
4. **NotificationBanner.js** - Banner display logic (polling, display modes)
5. **TileCounter.js** - Tile counter logic (statistics, color coding)
6. **i18n.properties** - All UI text translations (90+ keys)

⚠️ **If any of these 6 files are missing, the application will NOT work!**

---

## 🔍 Verification After Import

### Check File Count
```
SE80 → Display BSP Application: ZNOTIFY_BANNER
Expand: MIME Objects

Expected Structure:
├── MIME Objects (8 root files)
│   ├── controller (9 files)
│   ├── css (1 file)
│   ├── i18n (1 file)
│   ├── model (3 files)
│   └── view (1 file)

Total: 23 files
```

### Test Application URL
```
https://your-system:port/sap/bc/bsp/sap/znotify_banner/index.html
```

Expected: Application loads without errors

### Check MIME Types (auto-detected by SE80)
- **.js** files → application/javascript
- **.json** files → application/json
- **.html** files → text/html
- **.css** files → text/css
- **.xml** files → application/xml
- **.properties** files → text/plain

---

## 🐛 Common Issues

### Issue: "Cannot find Component-preload.js"
**Solution**: Check file is in MIME Objects root (NOT in subfolder)

### Issue: "i18n keys not translated"
**Solution**: Ensure i18n/i18n.properties was imported correctly

### Issue: "Banner not displayed"
**Solution**: Check controller/NotificationBanner.js is present and activated

### Issue: "Tile counter shows 0"
**Solution**: Check controller/TileCounter.js is present and REST API is working

---

## ⏱️ Estimated Time

- **Minimum (only critical files)**: 10 minutes
- **Full import (all 23 files)**: 20-30 minutes
- **With multi-select**: 15-20 minutes
- **File-by-file**: 30-40 minutes

---

**Last Updated**: October 1, 2025
**Version**: 1.1.0
