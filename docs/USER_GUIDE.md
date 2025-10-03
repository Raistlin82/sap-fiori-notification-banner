# 👤 User Guide - SAP Fiori Global Notification Banner

**For end users of SAP Fiori applications**

---

## 📋 Table of Contents
1. [Introduction](#introduction)
2. [How It Works](#how-it-works)
3. [Notification Types](#notification-types)
4. [User Interactions](#user-interactions)
5. [Supported Devices](#supported-devices)
6. [Accessibility](#accessibility)
7. [FAQ](#faq)

---

## 🎯 Introduction

The **Global Notification Banner** is a system that automatically displays important messages at the top of all SAP Fiori applications you use. You don't need to install anything - the banner appears automatically when there are active notifications.

### What You'll See
- Colored banners with important messages
- Icons indicating the message type
- Buttons to navigate and close
- Responsive design on all devices

---

## 🔄 How It Works

### Automatic Display
The banner appears automatically when:
- You access any Fiori application
- New notifications are published
- The system finds active messages for your user

### Real-time Updates
- The system checks every **30 seconds** for new notifications
- Expired notifications disappear automatically
- No need to reload the page

---

## 🎨 Notification Types

### 🔴 **High Priority (Critical)**
- **Color**: Red with dark red border
- **Usage**: Critical issues, emergencies, system down
- **Example**: *"System under extraordinary maintenance from 2:00 PM to 4:00 PM"*
- **Action**: Read immediately and take note

### 🟡 **Medium Priority (Warning)**
- **Color**: Yellow/Orange
- **Usage**: Important warnings, scheduled changes
- **Example**: *"New feature available from next Monday"*
- **Action**: Read when possible

### 🔵 **Low Priority (Informative)**
- **Color**: Blue
- **Usage**: General information, tips
- **Example**: *"Remember to update your profile data"*
- **Action**: Read when you have time

### 🟢 **Success**
- **Color**: Green
- **Usage**: Positive confirmations, completions
- **Example**: *"Data migration completed successfully"*

### ⚪ **Maintenance**
- **Color**: Gray
- **Usage**: Scheduled maintenance
- **Example**: *"Planned maintenance Sunday night"*

---

## 🖱️ User Interactions

### Basic Display

**Single Notification Banner:**

| ⚠️ **NOTIFICATION TITLE**: Important message text | **❌** |
|---------------------------------------------------|--------|

### With Multiple Notifications

**Multiple Notifications Banner (with navigation):**

| ⚠️ **TITLE**: Message | **⬅️** [1 of 3] **➡️** | **❌** |
|----------------------|------------------------|--------|

### Available Actions

#### ❌ **Close Notification**
- **What it does**: Removes the current notification from display
- **How**: Click the **X** on the right
- **Note**: The notification won't be shown to you again, but remains active for other users

#### ⬅️ **Previous Notification**
- **What it does**: Shows the previous notification
- **How**: Click the **left arrow**
- **Available**: Only if there are multiple notifications

#### ➡️ **Next Notification**
- **What it does**: Shows the next notification
- **How**: Click the **right arrow**
- **Available**: Only if there are multiple notifications

#### 📊 **Counter**
- **What it shows**: Current position (e.g., "1 of 3")
- **Utility**: Understand how many total notifications there are

---

## 📱 Supported Devices

### 🖥️ **Desktop**
- **Display**: Full banner with all controls
- **Position**: Fixed at top of page
- **Interaction**: Mouse click

### 📱 **Tablet**
- **Display**: Adaptive layout, essential controls
- **Position**: Fixed at top, adapts to width
- **Interaction**: Touch optimized

### 📱 **Smartphone**
- **Display**: Compact, text wraps if needed
- **Controls**: Simplified (hides counter on small screens)
- **Interaction**: Touch friendly with larger targets

### Supported Browsers
- ✅ **Chrome 90+**
- ✅ **Firefox 88+**
- ✅ **Safari 14+**
- ✅ **Edge 90+**
- ⚠️ **Internet Explorer 11** (limited support)

---

## ♿ Accessibility

### Screen Reader
- **Full support** for NVDA, JAWS, VoiceOver
- **Automatic announcements** when new notifications appear
- **Full keyboard navigation**

### Keyboard Navigation
- **Tab**: Navigate between controls
- **Enter/Space**: Activate buttons
- **Escape**: Close notification
- **Arrows**: Navigate between multiple notifications

### High Contrast
- **Automatic support** for system high contrast mode
- **Thicker borders** and contrasting colors
- **Compatible** with Windows/Mac accessibility settings

### Reduced Motion
- **Animations disabled** if set in system
- **Simplified transitions** for motion-sensitive users

---

## ❓ FAQ - Frequently Asked Questions

### **Q: The banner doesn't appear, what should I do?**
**A**: Verify that:
- You are logged in correctly to SAP
- Your browser is supported
- No popup blockers are active
- Contact the administrator if the problem persists

### **Q: Can I disable notifications?**
**A**: No, notifications are managed centrally by administrators to ensure that critical information reaches all users.

### **Q: I closed a notification by mistake, how can I see it again?**
**A**: Once closed, the notification won't reappear for your user. Contact the administrator if you need to review the content.

### **Q: Why do some notifications have different priorities?**
**A**: Priorities help distinguish:
- **High**: Immediate actions required
- **Medium**: Important information
- **Low**: General information

### **Q: Do notifications work on all Fiori apps?**
**A**: Yes, the banner appears automatically on all Fiori applications in the system.

### **Q: What happens if I'm offline?**
**A**: Notifications are loaded when you come back online. The system checks automatically every 30 seconds.

### **Q: Can I see old notifications?**
**A**: No, only active notifications within the current period are shown. Expired notifications are no longer visible.

### **Q: The banner covers important content, what should I do?**
**A**: The banner is designed not to interfere, but you can:
- Close the notification if no longer relevant
- Scroll the page to see content underneath

### **Q: Is the banner different on mobile?**
**A**: Yes, it's optimized for small screens:
- More compact layout
- Touch-friendly controls
- Text adapts to width

### **Q: Can I report problems with a notification?**
**A**: Contact the system administrator or IT team directly for problems with content or functionality.

---

## 📞 Support

### Contacts
- **Technical Support**: [Gabriele Rendina - gabriele.rendina@lutech.it](mailto:gabriele.rendina@lutech.it)
- **Functional Support**: [Ileana Scaglia - ileana.scaglia@lutech.it](mailto:ileana.scaglia@lutech.it)

### Reporting Issues
For technical problems, always include:
- Browser used and version
- Device (desktop/mobile/tablet)
- Screenshot if possible
- Date and time of the problem

---

**📱 For the best experience, keep your browser updated and enable JavaScript.**