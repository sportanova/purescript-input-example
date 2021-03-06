(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
(function (global){
var topLevel = typeof global !== 'undefined' ? global :
    typeof window !== 'undefined' ? window : {}
var minDoc = require('min-document');

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"min-document":208}],2:[function(require,module,exports){
"use strict";

module.exports = function isObject(x) {
	return typeof x === "object" && x !== null;
};

},{}],3:[function(require,module,exports){
var createElement = require("./vdom/create-element.js")

module.exports = createElement

},{"./vdom/create-element.js":7}],4:[function(require,module,exports){
var diff = require("./vtree/diff.js")

module.exports = diff

},{"./vtree/diff.js":24}],5:[function(require,module,exports){
var patch = require("./vdom/patch.js")

module.exports = patch

},{"./vdom/patch.js":10}],6:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook.js")

module.exports = applyProperties

function applyProperties(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName]

        if (propValue === undefined) {
            removeProperty(node, propName, propValue, previous);
        } else if (isHook(propValue)) {
            removeProperty(node, propName, propValue, previous)
            if (propValue.hook) {
                propValue.hook(node,
                    propName,
                    previous ? previous[propName] : undefined)
            }
        } else {
            if (isObject(propValue)) {
                patchObject(node, props, previous, propName, propValue);
            } else {
                node[propName] = propValue
            }
        }
    }
}

function removeProperty(node, propName, propValue, previous) {
    if (previous) {
        var previousValue = previous[propName]

        if (!isHook(previousValue)) {
            if (propName === "attributes") {
                for (var attrName in previousValue) {
                    node.removeAttribute(attrName)
                }
            } else if (propName === "style") {
                for (var i in previousValue) {
                    node.style[i] = ""
                }
            } else if (typeof previousValue === "string") {
                node[propName] = ""
            } else {
                node[propName] = null
            }
        } else if (previousValue.unhook) {
            previousValue.unhook(node, propName, propValue)
        }
    }
}

function patchObject(node, props, previous, propName, propValue) {
    var previousValue = previous ? previous[propName] : undefined

    // Set attributes
    if (propName === "attributes") {
        for (var attrName in propValue) {
            var attrValue = propValue[attrName]

            if (attrValue === undefined) {
                node.removeAttribute(attrName)
            } else {
                node.setAttribute(attrName, attrValue)
            }
        }

        return
    }

    if(previousValue && isObject(previousValue) &&
        getPrototype(previousValue) !== getPrototype(propValue)) {
        node[propName] = propValue
        return
    }

    if (!isObject(node[propName])) {
        node[propName] = {}
    }

    var replacer = propName === "style" ? "" : undefined

    for (var k in propValue) {
        var value = propValue[k]
        node[propName][k] = (value === undefined) ? replacer : value
    }
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

},{"../vnode/is-vhook.js":15,"is-object":2}],7:[function(require,module,exports){
var document = require("global/document")

var applyProperties = require("./apply-properties")

var isVNode = require("../vnode/is-vnode.js")
var isVText = require("../vnode/is-vtext.js")
var isWidget = require("../vnode/is-widget.js")
var handleThunk = require("../vnode/handle-thunk.js")

module.exports = createElement

function createElement(vnode, opts) {
    var doc = opts ? opts.document || document : document
    var warn = opts ? opts.warn : null

    vnode = handleThunk(vnode).a

    if (isWidget(vnode)) {
        return vnode.init()
    } else if (isVText(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode)
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName)

    var props = vnode.properties
    applyProperties(node, props)

    var children = vnode.children

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement(children[i], opts)
        if (childNode) {
            node.appendChild(childNode)
        }
    }

    return node
}

},{"../vnode/handle-thunk.js":13,"../vnode/is-vnode.js":16,"../vnode/is-vtext.js":17,"../vnode/is-widget.js":18,"./apply-properties":6,"global/document":1}],8:[function(require,module,exports){
// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {}

module.exports = domIndex

function domIndex(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending)
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {}


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode
        }

        var vChildren = tree.children

        if (vChildren) {

            var childNodes = rootNode.childNodes

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1

                var vChild = vChildren[i] || noChild
                var nextIndex = rootIndex + (vChild.count || 0)

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
                }

                rootIndex = nextIndex
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0
    var maxIndex = indices.length - 1
    var currentIndex
    var currentItem

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0
        currentItem = indices[currentIndex]

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

},{}],9:[function(require,module,exports){
var applyProperties = require("./apply-properties")

var isWidget = require("../vnode/is-widget.js")
var VPatch = require("../vnode/vpatch.js")

var updateWidget = require("./update-widget")

module.exports = applyPatch

function applyPatch(vpatch, domNode, renderOptions) {
    var type = vpatch.type
    var vNode = vpatch.vNode
    var patch = vpatch.patch

    switch (type) {
        case VPatch.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch.ORDER:
            reorderChildren(domNode, patch)
            return domNode
        case VPatch.PROPS:
            applyProperties(domNode, patch, vNode.properties)
            return domNode
        case VPatch.THUNK:
            return replaceRoot(domNode,
                renderOptions.patch(domNode, patch, renderOptions))
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode

    if (parentNode) {
        parentNode.removeChild(domNode)
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = renderOptions.render(vNode, renderOptions)

    if (parentNode) {
        parentNode.appendChild(newNode)
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text)
        newNode = domNode
    } else {
        var parentNode = domNode.parentNode
        newNode = renderOptions.render(vText, renderOptions)

        if (parentNode && newNode !== domNode) {
            parentNode.replaceChild(newNode, domNode)
        }
    }

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    var updating = updateWidget(leftVNode, widget)
    var newNode

    if (updating) {
        newNode = widget.update(leftVNode, domNode) || domNode
    } else {
        newNode = renderOptions.render(widget, renderOptions)
    }

    var parentNode = domNode.parentNode

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    if (!updating) {
        destroyWidget(domNode, leftVNode)
    }

    return newNode
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode
    var newNode = renderOptions.render(vNode, renderOptions)

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget(w)) {
        w.destroy(domNode)
    }
}

function reorderChildren(domNode, moves) {
    var childNodes = domNode.childNodes
    var keyMap = {}
    var node
    var remove
    var insert

    for (var i = 0; i < moves.removes.length; i++) {
        remove = moves.removes[i]
        node = childNodes[remove.from]
        if (remove.key) {
            keyMap[remove.key] = node
        }
        domNode.removeChild(node)
    }

    var length = childNodes.length
    for (var j = 0; j < moves.inserts.length; j++) {
        insert = moves.inserts[j]
        node = keyMap[insert.key]
        // this is the weirdest bug i've ever seen in webkit
        domNode.insertBefore(node, insert.to >= length++ ? null : childNodes[insert.to])
    }
}

function replaceRoot(oldRoot, newRoot) {
    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
        oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }

    return newRoot;
}

},{"../vnode/is-widget.js":18,"../vnode/vpatch.js":21,"./apply-properties":6,"./update-widget":11}],10:[function(require,module,exports){
var document = require("global/document")
var isArray = require("x-is-array")

var render = require("./create-element")
var domIndex = require("./dom-index")
var patchOp = require("./patch-op")
module.exports = patch

function patch(rootNode, patches, renderOptions) {
    renderOptions = renderOptions || {}
    renderOptions.patch = renderOptions.patch && renderOptions.patch !== patch
        ? renderOptions.patch
        : patchRecursive
    renderOptions.render = renderOptions.render || render

    return renderOptions.patch(rootNode, patches, renderOptions)
}

function patchRecursive(rootNode, patches, renderOptions) {
    var indices = patchIndices(patches)

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices)
    var ownerDocument = rootNode.ownerDocument

    if (!renderOptions.document && ownerDocument !== document) {
        renderOptions.document = ownerDocument
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i]
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions)
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode

    if (isArray(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions)

            if (domNode === rootNode) {
                rootNode = newNode
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions)

        if (domNode === rootNode) {
            rootNode = newNode
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = []

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key))
        }
    }

    return indices
}

},{"./create-element":7,"./dom-index":8,"./patch-op":9,"global/document":1,"x-is-array":25}],11:[function(require,module,exports){
var isWidget = require("../vnode/is-widget.js")

module.exports = updateWidget

function updateWidget(a, b) {
    if (isWidget(a) && isWidget(b)) {
        if ("name" in a && "name" in b) {
            return a.id === b.id
        } else {
            return a.init === b.init
        }
    }

    return false
}

},{"../vnode/is-widget.js":18}],12:[function(require,module,exports){
'use strict';

module.exports = SoftSetHook;

function SoftSetHook(value) {
    if (!(this instanceof SoftSetHook)) {
        return new SoftSetHook(value);
    }

    this.value = value;
}

SoftSetHook.prototype.hook = function (node, propertyName) {
    if (node[propertyName] !== this.value) {
        node[propertyName] = this.value;
    }
};

},{}],13:[function(require,module,exports){
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    var renderedA = a
    var renderedB = b

    if (isThunk(b)) {
        renderedB = renderThunk(b, a)
    }

    if (isThunk(a)) {
        renderedA = renderThunk(a, null)
    }

    return {
        a: renderedA,
        b: renderedB
    }
}

function renderThunk(thunk, previous) {
    var renderedThunk = thunk.vnode

    if (!renderedThunk) {
        renderedThunk = thunk.vnode = thunk.render(previous)
    }

    if (!(isVNode(renderedThunk) ||
            isVText(renderedThunk) ||
            isWidget(renderedThunk))) {
        throw new Error("thunk did not return a valid node");
    }

    return renderedThunk
}

},{"./is-thunk":14,"./is-vnode":16,"./is-vtext":17,"./is-widget":18}],14:[function(require,module,exports){
module.exports = isThunk

function isThunk(t) {
    return t && t.type === "Thunk"
}

},{}],15:[function(require,module,exports){
module.exports = isHook

function isHook(hook) {
    return hook &&
      (typeof hook.hook === "function" && !hook.hasOwnProperty("hook") ||
       typeof hook.unhook === "function" && !hook.hasOwnProperty("unhook"))
}

},{}],16:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version
}

},{"./version":19}],17:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualText

function isVirtualText(x) {
    return x && x.type === "VirtualText" && x.version === version
}

},{"./version":19}],18:[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && w.type === "Widget"
}

},{}],19:[function(require,module,exports){
module.exports = "2"

},{}],20:[function(require,module,exports){
var version = require("./version")
var isVNode = require("./is-vnode")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")
var isVHook = require("./is-vhook")

module.exports = VirtualNode

var noProperties = {}
var noChildren = []

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName
    this.properties = properties || noProperties
    this.children = children || noChildren
    this.key = key != null ? String(key) : undefined
    this.namespace = (typeof namespace === "string") ? namespace : null

    var count = (children && children.length) || 0
    var descendants = 0
    var hasWidgets = false
    var hasThunks = false
    var descendantHooks = false
    var hooks

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName]
            if (isVHook(property) && property.unhook) {
                if (!hooks) {
                    hooks = {}
                }

                hooks[propName] = property
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i]
        if (isVNode(child)) {
            descendants += child.count || 0

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true
            }

            if (!hasThunks && child.hasThunks) {
                hasThunks = true
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true
            }
        } else if (!hasWidgets && isWidget(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true
            }
        } else if (!hasThunks && isThunk(child)) {
            hasThunks = true;
        }
    }

    this.count = count + descendants
    this.hasWidgets = hasWidgets
    this.hasThunks = hasThunks
    this.hooks = hooks
    this.descendantHooks = descendantHooks
}

VirtualNode.prototype.version = version
VirtualNode.prototype.type = "VirtualNode"

},{"./is-thunk":14,"./is-vhook":15,"./is-vnode":16,"./is-widget":18,"./version":19}],21:[function(require,module,exports){
var version = require("./version")

VirtualPatch.NONE = 0
VirtualPatch.VTEXT = 1
VirtualPatch.VNODE = 2
VirtualPatch.WIDGET = 3
VirtualPatch.PROPS = 4
VirtualPatch.ORDER = 5
VirtualPatch.INSERT = 6
VirtualPatch.REMOVE = 7
VirtualPatch.THUNK = 8

module.exports = VirtualPatch

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type)
    this.vNode = vNode
    this.patch = patch
}

VirtualPatch.prototype.version = version
VirtualPatch.prototype.type = "VirtualPatch"

},{"./version":19}],22:[function(require,module,exports){
var version = require("./version")

module.exports = VirtualText

function VirtualText(text) {
    this.text = String(text)
}

VirtualText.prototype.version = version
VirtualText.prototype.type = "VirtualText"

},{"./version":19}],23:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook")

module.exports = diffProps

function diffProps(a, b) {
    var diff

    for (var aKey in a) {
        if (!(aKey in b)) {
            diff = diff || {}
            diff[aKey] = undefined
        }

        var aValue = a[aKey]
        var bValue = b[aKey]

        if (aValue === bValue) {
            continue
        } else if (isObject(aValue) && isObject(bValue)) {
            if (getPrototype(bValue) !== getPrototype(aValue)) {
                diff = diff || {}
                diff[aKey] = bValue
            } else if (isHook(bValue)) {
                 diff = diff || {}
                 diff[aKey] = bValue
            } else {
                var objectDiff = diffProps(aValue, bValue)
                if (objectDiff) {
                    diff = diff || {}
                    diff[aKey] = objectDiff
                }
            }
        } else {
            diff = diff || {}
            diff[aKey] = bValue
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {}
            diff[bKey] = b[bKey]
        }
    }

    return diff
}

function getPrototype(value) {
  if (Object.getPrototypeOf) {
    return Object.getPrototypeOf(value)
  } else if (value.__proto__) {
    return value.__proto__
  } else if (value.constructor) {
    return value.constructor.prototype
  }
}

},{"../vnode/is-vhook":15,"is-object":2}],24:[function(require,module,exports){
var isArray = require("x-is-array")

var VPatch = require("../vnode/vpatch")
var isVNode = require("../vnode/is-vnode")
var isVText = require("../vnode/is-vtext")
var isWidget = require("../vnode/is-widget")
var isThunk = require("../vnode/is-thunk")
var handleThunk = require("../vnode/handle-thunk")

var diffProps = require("./diff-props")

module.exports = diff

function diff(a, b) {
    var patch = { a: a }
    walk(a, b, patch, 0)
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        return
    }

    var apply = patch[index]
    var applyClear = false

    if (isThunk(a) || isThunk(b)) {
        thunks(a, b, patch, index)
    } else if (b == null) {

        // If a is a widget we will add a remove patch for it
        // Otherwise any child widgets/hooks must be destroyed.
        // This prevents adding two remove patches for a widget.
        if (!isWidget(a)) {
            clearState(a, patch, index)
            apply = patch[index]
        }

        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties)
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch))
                }
                apply = diffChildren(a, b, patch, apply, index)
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
                applyClear = true
            }
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
            applyClear = true
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
            applyClear = true
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
        }
    } else if (isWidget(b)) {
        if (!isWidget(a)) {
            applyClear = true
        }

        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))
    }

    if (apply) {
        patch[index] = apply
    }

    if (applyClear) {
        clearState(a, patch, index)
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children
    var orderedSet = reorder(aChildren, b.children)
    var bChildren = orderedSet.children

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i]
        var rightNode = bChildren[i]
        index += 1

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply,
                    new VPatch(VPatch.INSERT, null, rightNode))
            }
        } else {
            walk(leftNode, rightNode, patch, index)
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count
        }
    }

    if (orderedSet.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(
            VPatch.ORDER,
            a,
            orderedSet.moves
        ))
    }

    return apply
}

function clearState(vNode, patch, index) {
    // TODO: Make this a single walk, not two
    unhook(vNode, patch, index)
    destroyWidgets(vNode, patch, index)
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(VPatch.REMOVE, vNode, null)
            )
        }
    } else if (isVNode(vNode) && (vNode.hasWidgets || vNode.hasThunks)) {
        var children = vNode.children
        var len = children.length
        for (var i = 0; i < len; i++) {
            var child = children[i]
            index += 1

            destroyWidgets(child, patch, index)

            if (isVNode(child) && child.count) {
                index += child.count
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

// Create a sub-patch for thunks
function thunks(a, b, patch, index) {
    var nodes = handleThunk(a, b)
    var thunkPatch = diff(nodes.a, nodes.b)
    if (hasPatches(thunkPatch)) {
        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch)
    }
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true
        }
    }

    return false
}

// Execute hooks when two nodes are identical
function unhook(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(
                    VPatch.PROPS,
                    vNode,
                    undefinedKeys(vNode.hooks)
                )
            )
        }

        if (vNode.descendantHooks || vNode.hasThunks) {
            var children = vNode.children
            var len = children.length
            for (var i = 0; i < len; i++) {
                var child = children[i]
                index += 1

                unhook(child, patch, index)

                if (isVNode(child) && child.count) {
                    index += child.count
                }
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

function undefinedKeys(obj) {
    var result = {}

    for (var key in obj) {
        result[key] = undefined
    }

    return result
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {
    // O(M) time, O(M) memory
    var bChildIndex = keyIndex(bChildren)
    var bKeys = bChildIndex.keys
    var bFree = bChildIndex.free

    if (bFree.length === bChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(N) time, O(N) memory
    var aChildIndex = keyIndex(aChildren)
    var aKeys = aChildIndex.keys
    var aFree = aChildIndex.free

    if (aFree.length === aChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(MAX(N, M)) memory
    var newChildren = []

    var freeIndex = 0
    var freeCount = bFree.length
    var deletedItems = 0

    // Iterate through a and match a node in b
    // O(N) time,
    for (var i = 0 ; i < aChildren.length; i++) {
        var aItem = aChildren[i]
        var itemIndex

        if (aItem.key) {
            if (bKeys.hasOwnProperty(aItem.key)) {
                // Match up the old keys
                itemIndex = bKeys[aItem.key]
                newChildren.push(bChildren[itemIndex])

            } else {
                // Remove old keyed items
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        } else {
            // Match the item in a with the next free item in b
            if (freeIndex < freeCount) {
                itemIndex = bFree[freeIndex++]
                newChildren.push(bChildren[itemIndex])
            } else {
                // There are no free items in b to match with
                // the free items in a, so the extra free nodes
                // are deleted.
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        }
    }

    var lastFreeIndex = freeIndex >= bFree.length ?
        bChildren.length :
        bFree[freeIndex]

    // Iterate through b and append any new keys
    // O(M) time
    for (var j = 0; j < bChildren.length; j++) {
        var newItem = bChildren[j]

        if (newItem.key) {
            if (!aKeys.hasOwnProperty(newItem.key)) {
                // Add any new keyed items
                // We are adding new items to the end and then sorting them
                // in place. In future we should insert new items in place.
                newChildren.push(newItem)
            }
        } else if (j >= lastFreeIndex) {
            // Add any leftover non-keyed items
            newChildren.push(newItem)
        }
    }

    var simulate = newChildren.slice()
    var simulateIndex = 0
    var removes = []
    var inserts = []
    var simulateItem

    for (var k = 0; k < bChildren.length;) {
        var wantedItem = bChildren[k]
        simulateItem = simulate[simulateIndex]

        // remove items
        while (simulateItem === null && simulate.length) {
            removes.push(remove(simulate, simulateIndex, null))
            simulateItem = simulate[simulateIndex]
        }

        if (!simulateItem || simulateItem.key !== wantedItem.key) {
            // if we need a key in this position...
            if (wantedItem.key) {
                if (simulateItem && simulateItem.key) {
                    // if an insert doesn't put this key in place, it needs to move
                    if (bKeys[simulateItem.key] !== k + 1) {
                        removes.push(remove(simulate, simulateIndex, simulateItem.key))
                        simulateItem = simulate[simulateIndex]
                        // if the remove didn't put the wanted item in place, we need to insert it
                        if (!simulateItem || simulateItem.key !== wantedItem.key) {
                            inserts.push({key: wantedItem.key, to: k})
                        }
                        // items are matching, so skip ahead
                        else {
                            simulateIndex++
                        }
                    }
                    else {
                        inserts.push({key: wantedItem.key, to: k})
                    }
                }
                else {
                    inserts.push({key: wantedItem.key, to: k})
                }
                k++
            }
            // a key in simulate has no matching wanted key, remove it
            else if (simulateItem && simulateItem.key) {
                removes.push(remove(simulate, simulateIndex, simulateItem.key))
            }
        }
        else {
            simulateIndex++
            k++
        }
    }

    // remove all the remaining nodes from simulate
    while(simulateIndex < simulate.length) {
        simulateItem = simulate[simulateIndex]
        removes.push(remove(simulate, simulateIndex, simulateItem && simulateItem.key))
    }

    // If the only moves we have are deletes then we can just
    // let the delete patch remove these items.
    if (removes.length === deletedItems && !inserts.length) {
        return {
            children: newChildren,
            moves: null
        }
    }

    return {
        children: newChildren,
        moves: {
            removes: removes,
            inserts: inserts
        }
    }
}

function remove(arr, index, key) {
    arr.splice(index, 1)

    return {
        from: index,
        key: key
    }
}

function keyIndex(children) {
    var keys = {}
    var free = []
    var length = children.length

    for (var i = 0; i < length; i++) {
        var child = children[i]

        if (child.key) {
            keys[child.key] = i
        } else {
            free.push(i)
        }
    }

    return {
        keys: keys,     // A hash of key name to index
        free: free      // An array of unkeyed item indices
    }
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray(apply)) {
            apply.push(patch)
        } else {
            apply = [apply, patch]
        }

        return apply
    } else {
        return patch
    }
}

},{"../vnode/handle-thunk":13,"../vnode/is-thunk":14,"../vnode/is-vnode":16,"../vnode/is-vtext":17,"../vnode/is-widget":18,"../vnode/vpatch":21,"./diff-props":23,"x-is-array":25}],25:[function(require,module,exports){
var nativeIsArray = Array.isArray
var toString = Object.prototype.toString

module.exports = nativeIsArray || isArray

function isArray(obj) {
    return toString.call(obj) === "[object Array]"
}

},{}],26:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Alt = function (__superclass_Prelude$dotFunctor_0, alt) {
    this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    this.alt = alt;
};
var altArray = new Alt(function () {
    return Prelude.functorArray;
}, Prelude.append(Prelude.semigroupArray));
var alt = function (dict) {
    return dict.alt;
};
var $less$bar$greater = function (dictAlt) {
    return alt(dictAlt);
};
module.exports = {
    Alt: Alt, 
    "<|>": $less$bar$greater, 
    alt: alt, 
    altArray: altArray
};

},{"../Prelude":203}],27:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Plus = require("../Control.Plus");
var Alternative = function (__superclass_Control$dotPlus$dotPlus_1, __superclass_Prelude$dotApplicative_0) {
    this["__superclass_Control.Plus.Plus_1"] = __superclass_Control$dotPlus$dotPlus_1;
    this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
};
var alternativeArray = new Alternative(function () {
    return Control_Plus.plusArray;
}, function () {
    return Prelude.applicativeArray;
});
module.exports = {
    Alternative: Alternative, 
    alternativeArray: alternativeArray
};

},{"../Control.Plus":77,"../Prelude":203}],28:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var $less$times = function (dictApply) {
    return function (a) {
        return function (b) {
            return Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Prelude["const"])(a))(b);
        };
    };
};
var $times$greater = function (dictApply) {
    return function (a) {
        return function (b) {
            return Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Prelude["const"](Prelude.id(Prelude.categoryFn)))(a))(b);
        };
    };
};
var lift5 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(f)(a))(b))(c))(d))(e);
                        };
                    };
                };
            };
        };
    };
};
var lift4 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(f)(a))(b))(c))(d);
                    };
                };
            };
        };
    };
};
var lift3 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(f)(a))(b))(c);
                };
            };
        };
    };
};
var lift2 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(f)(a))(b);
            };
        };
    };
};
module.exports = {
    lift5: lift5, 
    lift4: lift4, 
    lift3: lift3, 
    lift2: lift2, 
    "*>": $times$greater, 
    "<*": $less$times
};

},{"../Prelude":203}],29:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Biapply = require("../Control.Biapply");
var Biapplicative = function (__superclass_Control$dotBiapply$dotBiapply_0, bipure) {
    this["__superclass_Control.Biapply.Biapply_0"] = __superclass_Control$dotBiapply$dotBiapply_0;
    this.bipure = bipure;
};
var bipure = function (dict) {
    return dict.bipure;
};
module.exports = {
    Biapplicative: Biapplicative, 
    bipure: bipure
};

},{"../Control.Biapply":30,"../Prelude":203}],30:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Bifunctor = require("../Data.Bifunctor");
var Biapply = function (__superclass_Data$dotBifunctor$dotBifunctor_0, biapply) {
    this["__superclass_Data.Bifunctor.Bifunctor_0"] = __superclass_Data$dotBifunctor$dotBifunctor_0;
    this.biapply = biapply;
};
var $less$less$dollar$greater$greater = Prelude.id(Prelude.categoryFn);
var biapply = function (dict) {
    return dict.biapply;
};
var $less$less$times$greater$greater = function (dictBiapply) {
    return biapply(dictBiapply);
};
var bilift2 = function (dictBiapply) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return $less$less$times$greater$greater(dictBiapply)($less$less$dollar$greater$greater(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(f)(g))(a))(b);
                };
            };
        };
    };
};
var bilift3 = function (dictBiapply) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return $less$less$times$greater$greater(dictBiapply)($less$less$times$greater$greater(dictBiapply)($less$less$dollar$greater$greater(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(f)(g))(a))(b))(c);
                    };
                };
            };
        };
    };
};
var $times$greater$greater = function (dictBiapply) {
    return function (a) {
        return function (b) {
            return $less$less$times$greater$greater(dictBiapply)($less$less$dollar$greater$greater(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(Prelude["const"](Prelude.id(Prelude.categoryFn)))(Prelude["const"](Prelude.id(Prelude.categoryFn))))(a))(b);
        };
    };
};
var $less$less$times = function (dictBiapply) {
    return function (a) {
        return function (b) {
            return $less$less$times$greater$greater(dictBiapply)($less$less$dollar$greater$greater(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(Prelude["const"])(Prelude["const"]))(a))(b);
        };
    };
};
module.exports = {
    Biapply: Biapply, 
    bilift3: bilift3, 
    bilift2: bilift2, 
    "<<*": $less$less$times, 
    "*>>": $times$greater$greater, 
    "<<*>>": $less$less$times$greater$greater, 
    biapply: biapply, 
    "<<$>>": $less$less$dollar$greater$greater
};

},{"../Data.Bifunctor":104,"../Prelude":203}],31:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var $greater$eq$greater = function (dictBind) {
    return function (f) {
        return function (g) {
            return function (a) {
                return Prelude[">>="](dictBind)(f(a))(g);
            };
        };
    };
};
var $eq$less$less = function (dictBind) {
    return function (f) {
        return function (m) {
            return Prelude[">>="](dictBind)(m)(f);
        };
    };
};
var $less$eq$less = function (dictBind) {
    return function (f) {
        return function (g) {
            return function (a) {
                return $eq$less$less(dictBind)(f)(g(a));
            };
        };
    };
};
var join = function (dictBind) {
    return function (m) {
        return Prelude[">>="](dictBind)(m)(Prelude.id(Prelude.categoryFn));
    };
};
var ifM = function (dictBind) {
    return function (cond) {
        return function (t) {
            return function (f) {
                return Prelude[">>="](dictBind)(cond)(function (cond$prime) {
                    if (cond$prime) {
                        return t;
                    };
                    if (!cond$prime) {
                        return f;
                    };
                    throw new Error("Failed pattern match at Control.Bind line 45, column 35 - line 45, column 56: " + [ cond$prime.constructor.name ]);
                });
            };
        };
    };
};
module.exports = {
    ifM: ifM, 
    join: join, 
    "<=<": $less$eq$less, 
    ">=>": $greater$eq$greater, 
    "=<<": $eq$less$less
};

},{"../Prelude":203}],32:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Extend = require("../Control.Extend");
var Comonad = function (__superclass_Control$dotExtend$dotExtend_0, extract) {
    this["__superclass_Control.Extend.Extend_0"] = __superclass_Control$dotExtend$dotExtend_0;
    this.extract = extract;
};
var extract = function (dict) {
    return dict.extract;
};
module.exports = {
    Comonad: Comonad, 
    extract: extract
};

},{"../Control.Extend":36,"../Prelude":203}],33:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Control_Coroutine = require("../Control.Coroutine");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_AVar = require("../Control.Monad.Aff.AVar");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var produce = function (recv) {
    return Control_Monad_Free_Trans.hoistFreeT(Control_Coroutine.functorEmit)(Control_Monad_Aff.functorAff)(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffAff))(Prelude.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorEmit)(Control_Monad_Aff.monadAff))(Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorEmit))(Control_Monad_Aff.monadAff)(Control_Monad_Aff_AVar.makeVar))(function (v) {
        return Prelude.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorEmit)(Control_Monad_Aff.monadAff))(Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorEmit))(Control_Monad_Aff.monadAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(recv(function ($3) {
            return Control_Monad_Aff.runAff(Prelude["const"](Prelude["return"](Control_Monad_Eff.applicativeEff)(Prelude.unit)))(Prelude["return"](Control_Monad_Eff.applicativeEff))(Control_Monad_Aff_AVar.putVar(v)($3));
        }))))(function () {
            return Control_Coroutine.producer(Control_Monad_Aff.monadAff)(Control_Monad_Aff_AVar.takeVar(v));
        });
    }));
};
var produce$prime = function (dictMonadAff) {
    return function ($4) {
        return Control_Monad_Free_Trans.hoistFreeT(Control_Coroutine.functorEmit)(((((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Control_Monad_Aff_Class.liftAff(dictMonadAff))(produce($4));
    };
};
module.exports = {
    "produce'": produce$prime, 
    produce: produce
};

},{"../Control.Coroutine":35,"../Control.Monad.Aff":43,"../Control.Monad.Aff.AVar":39,"../Control.Monad.Aff.Class":40,"../Control.Monad.Eff":54,"../Control.Monad.Eff.Class":46,"../Control.Monad.Free.Trans":57,"../Control.Monad.Trans":71,"../Data.Either":114,"../Prelude":203}],34:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Coroutine = require("../Control.Coroutine");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Control_Monad_Maybe_Trans = require("../Control.Monad.Maybe.Trans");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Bind = require("../Control.Bind");
var Control_Plus = require("../Control.Plus");
var Data_Functor = require("../Data.Functor");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Either = require("../Data.Either");
var Data_Identity = require("../Data.Identity");
var Data_Maybe = require("../Data.Maybe");
var Emit = (function () {
    function Emit(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Emit.create = function (value0) {
        return function (value1) {
            return new Emit(value0, value1);
        };
    };
    return Emit;
})();
var Stall = (function () {
    function Stall(value0) {
        this.value0 = value0;
    };
    Stall.create = function (value0) {
        return new Stall(value0);
    };
    return Stall;
})();
var stallF = function (e) {
    return function (s) {
        return function (q) {
            if (q instanceof Emit) {
                return e(q.value0)(q.value1);
            };
            if (q instanceof Stall) {
                return s(q.value0);
            };
            throw new Error("Failed pattern match at Control.Coroutine.Stalling line 46, column 3 - line 50, column 1: " + [ q.constructor.name ]);
        };
    };
};
var runStallingProcess = function (dictMonadRec) {
    return function ($28) {
        return Control_Monad_Maybe_Trans.runMaybeT(Control_Monad_Free_Trans.runFreeT(Data_Maybe.functorMaybe)(Control_Monad_Maybe_Trans.monadRecMaybeT(dictMonadRec))(Data_Maybe.maybe(Control_Plus.empty(Control_Monad_Maybe_Trans.plusMaybeT(dictMonadRec["__superclass_Prelude.Monad_0"]())))(Prelude.pure(Control_Monad_Maybe_Trans.applicativeMaybeT(dictMonadRec["__superclass_Prelude.Monad_0"]()))))(Control_Monad_Free_Trans.hoistFreeT(Data_Maybe.functorMaybe)(Control_Monad_Maybe_Trans.functorMaybeT(dictMonadRec["__superclass_Prelude.Monad_0"]()))(function ($29) {
            return Control_Monad_Maybe_Trans.MaybeT(Prelude.map((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe.Just.create)($29));
        })($28)));
    };
};
var producerToStallingProducer = function (dictFunctor) {
    return Control_Monad_Free_Trans.interpret(Control_Coroutine.functorEmit)(dictFunctor)(function (v) {
        return new Emit(v.value0, v.value1);
    });
};
var processToStallingProcess = function (dictFunctor) {
    return Control_Monad_Free_Trans.interpret(Data_Identity.functorIdentity)(dictFunctor)(function ($30) {
        return Data_Maybe.Just.create(Data_Identity.runIdentity($30));
    });
};
var bifunctorStallF = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (q) {
            if (q instanceof Emit) {
                return new Emit(f(q.value0), g(q.value1));
            };
            if (q instanceof Stall) {
                return new Stall(g(q.value0));
            };
            throw new Error("Failed pattern match at Control.Coroutine.Stalling line 52, column 5 - line 56, column 1: " + [ q.constructor.name ]);
        };
    };
});
var functorStallF = new Prelude.Functor(function (f) {
    return Data_Bifunctor.rmap(bifunctorStallF)(f);
});
var emit = function (dictMonad) {
    return function ($31) {
        return Control_Monad_Free_Trans.liftFreeT(functorStallF)(dictMonad)(Prelude.flip(Emit.create)(Prelude.unit)($31));
    };
};
var catMaybes = function (dictMonadRec) {
    return Control_Monad_Rec_Class.tailRecM(Control_Monad_Free_Trans.monadRecFreeT(functorStallF)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(Control_Bind[">=>"](Control_Monad_Free_Trans.bindFreeT(functorStallF)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(function ($32) {
        return Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(functorStallF))(dictMonadRec["__superclass_Prelude.Monad_0"]())(Control_Monad_Free_Trans.resume(functorStallF)(dictMonadRec)($32));
    })(Data_Either.either(function ($33) {
        return Prelude.pure(Control_Monad_Free_Trans.applicativeFreeT(functorStallF)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(Data_Either.Right.create($33));
    })(stallF(function (mo) {
        return function (t) {
            return Data_Functor["$>"](Control_Monad_Free_Trans.functorFreeT(functorStallF)((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]()))(Data_Maybe.maybe(Prelude.pure(Control_Monad_Free_Trans.applicativeFreeT(functorStallF)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(Prelude.unit))(emit(dictMonadRec["__superclass_Prelude.Monad_0"]()))(mo))(new Data_Either.Left(t));
        };
    })(function ($34) {
        return Prelude.pure(Control_Monad_Free_Trans.applicativeFreeT(functorStallF)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(Data_Either.Left.create($34));
    }))));
};
var stall = function (dictMonad) {
    return Control_Monad_Free_Trans.liftFreeT(functorStallF)(dictMonad)(new Stall(Prelude.unit));
};
var $dollar$dollar$qmark = function (dictMonadRec) {
    return Control_Coroutine.fuseWith(functorStallF)(Control_Coroutine.functorAwait)(Data_Maybe.functorMaybe)(dictMonadRec)(function (f) {
        return function (q) {
            return function (v) {
                if (q instanceof Emit) {
                    return new Data_Maybe.Just(f(q.value1)(v(q.value0)));
                };
                if (q instanceof Stall) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Control.Coroutine.Stalling line 87, column 5 - line 91, column 1: " + [ q.constructor.name ]);
            };
        };
    });
};
var mapStallingProducer = function (dictFunctor) {
    return function ($35) {
        return Control_Monad_Free_Trans.interpret(functorStallF)(dictFunctor)(Data_Bifunctor.lmap(bifunctorStallF)($35));
    };
};
var filter = function (dictMonadRec) {
    return function (p) {
        return function ($36) {
            return catMaybes(dictMonadRec)(mapStallingProducer((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function (x) {
                var $27 = p(x);
                if ($27) {
                    return new Data_Maybe.Just(x);
                };
                if (!$27) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Control.Coroutine.Stalling line 150, column 5 - line 150, column 33: " + [ $27.constructor.name ]);
            })($36));
        };
    };
};
module.exports = {
    Emit: Emit, 
    Stall: Stall, 
    filter: filter, 
    catMaybes: catMaybes, 
    mapStallingProducer: mapStallingProducer, 
    "$$?": $dollar$dollar$qmark, 
    runStallingProcess: runStallingProcess, 
    processToStallingProcess: processToStallingProcess, 
    producerToStallingProducer: producerToStallingProducer, 
    stallF: stallF, 
    stall: stall, 
    emit: emit, 
    bifunctorStallF: bifunctorStallF, 
    functorStallF: functorStallF
};

},{"../Control.Bind":31,"../Control.Coroutine":35,"../Control.Monad.Free.Trans":57,"../Control.Monad.Maybe.Trans":60,"../Control.Monad.Rec.Class":65,"../Control.Monad.Trans":71,"../Control.Plus":77,"../Data.Bifunctor":104,"../Data.Either":114,"../Data.Functor":134,"../Data.Identity":137,"../Data.Maybe":152,"../Prelude":203}],35:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Tuple = require("../Data.Tuple");
var Data_Either = require("../Data.Either");
var Data_Identity = require("../Data.Identity");
var Data_Functor = require("../Data.Functor");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Profunctor = require("../Data.Profunctor");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Transform = function (x) {
    return x;
};
var Emit = (function () {
    function Emit(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Emit.create = function (value0) {
        return function (value1) {
            return new Emit(value0, value1);
        };
    };
    return Emit;
})();
var Await = function (x) {
    return x;
};
var runProcess = function (dictMonadRec) {
    return Control_Monad_Free_Trans.runFreeT(Data_Identity.functorIdentity)(dictMonadRec)(function ($104) {
        return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(Data_Identity.runIdentity($104));
    });
};
var profunctorAwait = new Data_Profunctor.Profunctor(function (f) {
    return function (g) {
        return function (v) {
            return Data_Profunctor.dimap(Data_Profunctor.profunctorFn)(f)(g)(v);
        };
    };
});
var loop = function (dictFunctor) {
    return function (dictMonad) {
        return function (me) {
            return Control_Monad_Rec_Class.tailRecM(Control_Monad_Free_Trans.monadRecFreeT(dictFunctor)(dictMonad))(function (v) {
                return Prelude.map(Control_Monad_Free_Trans.functorFreeT(dictFunctor)(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]()))(Data_Maybe.maybe(new Data_Either.Left(Prelude.unit))(Data_Either.Right.create))(me);
            })(Prelude.unit);
        };
    };
};
var fuseWith = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictFunctor2) {
            return function (dictMonadRec) {
                return function (zap) {
                    return function (fs) {
                        return function (gs) {
                            var go = function (v) {
                                return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(Control_Monad_Free_Trans.resume(dictFunctor1)(dictMonadRec)(v.value1))(function (v1) {
                                    return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(Control_Monad_Free_Trans.resume(dictFunctor)(dictMonadRec)(v.value0))(function (v2) {
                                        var $49 = Prelude["<*>"](Data_Either.applyEither)(Prelude["<$>"](Data_Either.functorEither)(zap(Data_Tuple.Tuple.create))(v2))(v1);
                                        if ($49 instanceof Data_Either.Left) {
                                            return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left($49.value0));
                                        };
                                        if ($49 instanceof Data_Either.Right) {
                                            return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(Prelude.map(dictFunctor2)(function (t) {
                                                return Control_Monad_Free_Trans.freeT(function (v3) {
                                                    return go(t);
                                                });
                                            })($49.value0)));
                                        };
                                        throw new Error("Failed pattern match at Control.Coroutine line 60, column 5 - line 64, column 1: " + [ $49.constructor.name ]);
                                    });
                                });
                            };
                            return Control_Monad_Free_Trans.freeT(function (v) {
                                return go(new Data_Tuple.Tuple(fs, gs));
                            });
                        };
                    };
                };
            };
        };
    };
};
var functorAwait = new Prelude.Functor(Data_Profunctor.rmap(profunctorAwait));
var $bslash$div = function (dictMonadRec) {
    return fuseWith(functorAwait)(functorAwait)(functorAwait)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                return function (v2) {
                    return f(v(v2.value0))(v1(v2.value1));
                };
            };
        };
    });
};
var bifunctorTransform = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (v) {
            return function ($105) {
                return Data_Bifunctor.bimap(Data_Tuple.bifunctorTuple)(f)(g)(v($105));
            };
        };
    };
});
var functorTransform = new Prelude.Functor(Data_Bifunctor.rmap(bifunctorTransform));
var transform = function (dictMonad) {
    return function (f) {
        return Control_Monad_Free_Trans.liftFreeT(functorTransform)(dictMonad)(function (i) {
            return new Data_Tuple.Tuple(f(i), Prelude.unit);
        });
    };
};
var $tilde$dollar = function (dictMonadRec) {
    return fuseWith(functorTransform)(functorAwait)(functorAwait)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                return function (i) {
                    var $66 = v(i);
                    return f($66.value1)(v1($66.value0));
                };
            };
        };
    });
};
var $tilde$tilde = function (dictMonadRec) {
    return fuseWith(functorTransform)(functorTransform)(functorTransform)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                return function (i) {
                    var $71 = v(i);
                    var $72 = v1($71.value0);
                    return new Data_Tuple.Tuple($72.value0, f($71.value1)($72.value1));
                };
            };
        };
    });
};
var bifunctorEmit = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (v) {
            return new Emit(f(v.value0), g(v.value1));
        };
    };
});
var functorEmit = new Prelude.Functor(Data_Bifunctor.rmap(bifunctorEmit));
var emit = function (dictMonad) {
    return function (o) {
        return Control_Monad_Free_Trans.liftFreeT(functorEmit)(dictMonad)(new Emit(o, Prelude.unit));
    };
};
var producer = function (dictMonad) {
    return function (recv) {
        return loop(functorEmit)(dictMonad)(Prelude.bind(Control_Monad_Free_Trans.bindFreeT(functorEmit)(dictMonad))(Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(functorEmit))(dictMonad)(recv))(function (v) {
            if (v instanceof Data_Either.Left) {
                return Data_Functor["$>"](Control_Monad_Free_Trans.functorFreeT(functorEmit)(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]()))(emit(dictMonad)(v.value0))(Data_Maybe.Nothing.value);
            };
            if (v instanceof Data_Either.Right) {
                return Prelude["return"](Control_Monad_Free_Trans.applicativeFreeT(functorEmit)(dictMonad))(new Data_Maybe.Just(v.value0));
            };
            throw new Error("Failed pattern match at Control.Coroutine line 86, column 3 - line 91, column 1: " + [ v.constructor.name ]);
        }));
    };
};
var $dollar$dollar = function (dictMonadRec) {
    return fuseWith(functorEmit)(functorAwait)(Data_Identity.functorIdentity)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                return f(v.value1)(v1(v.value0));
            };
        };
    });
};
var $dollar$tilde = function (dictMonadRec) {
    return fuseWith(functorEmit)(functorTransform)(functorEmit)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                var $92 = v1(v.value0);
                return new Emit($92.value0, f(v.value1)($92.value1));
            };
        };
    });
};
var $div$bslash = function (dictMonadRec) {
    return fuseWith(functorEmit)(functorEmit)(functorEmit)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                return new Emit(new Data_Tuple.Tuple(v.value0, v1.value0), f(v.value1)(v1.value1));
            };
        };
    });
};
var $$await = function (dictMonad) {
    return Control_Monad_Free_Trans.liftFreeT(functorAwait)(dictMonad)(Prelude.id(Prelude.categoryFn));
};
var consumer = function (dictMonad) {
    return function (send) {
        return loop(functorAwait)(dictMonad)(Prelude.bind(Control_Monad_Free_Trans.bindFreeT(functorAwait)(dictMonad))($$await(dictMonad))(function (v) {
            return Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(functorAwait))(dictMonad)(send(v));
        }));
    };
};
module.exports = {
    Transform: Transform, 
    Await: Await, 
    Emit: Emit, 
    "\\/": $bslash$div, 
    "/\\": $div$bslash, 
    "~~": $tilde$tilde, 
    "~$": $tilde$dollar, 
    "$~": $dollar$tilde, 
    "$$": $dollar$dollar, 
    transform: transform, 
    consumer: consumer, 
    "await": $$await, 
    producer: producer, 
    emit: emit, 
    fuseWith: fuseWith, 
    runProcess: runProcess, 
    loop: loop, 
    bifunctorEmit: bifunctorEmit, 
    functorEmit: functorEmit, 
    profunctorAwait: profunctorAwait, 
    functorAwait: functorAwait, 
    bifunctorTransform: bifunctorTransform, 
    functorTransform: functorTransform
};

},{"../Control.Monad.Free.Trans":57,"../Control.Monad.Rec.Class":65,"../Control.Monad.Trans":71,"../Data.Bifunctor":104,"../Data.Either":114,"../Data.Functor":134,"../Data.Identity":137,"../Data.Maybe":152,"../Data.Profunctor":164,"../Data.Tuple":171,"../Prelude":203}],36:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Extend = function (__superclass_Prelude$dotFunctor_0, extend) {
    this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    this.extend = extend;
};
var extendFn = function (dictSemigroup) {
    return new Extend(function () {
        return Prelude.functorFn;
    }, function (f) {
        return function (g) {
            return function (w) {
                return f(function (w$prime) {
                    return g(Prelude["<>"](dictSemigroup)(w)(w$prime));
                });
            };
        };
    });
};
var extend = function (dict) {
    return dict.extend;
};
var $less$less$eq = function (dictExtend) {
    return extend(dictExtend);
};
var $eq$less$eq = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return f($less$less$eq(dictExtend)(g)(w));
            };
        };
    };
};
var $eq$greater$eq = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return g($less$less$eq(dictExtend)(f)(w));
            };
        };
    };
};
var $eq$greater$greater = function (dictExtend) {
    return function (w) {
        return function (f) {
            return $less$less$eq(dictExtend)(f)(w);
        };
    };
};
var duplicate = function (dictExtend) {
    return extend(dictExtend)(Prelude.id(Prelude.categoryFn));
};
module.exports = {
    Extend: Extend, 
    duplicate: duplicate, 
    "=<=": $eq$less$eq, 
    "=>=": $eq$greater$eq, 
    "=>>": $eq$greater$greater, 
    "<<=": $less$less$eq, 
    extend: extend, 
    extendFn: extendFn
};

},{"../Prelude":203}],37:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Lazy = function (defer) {
    this.defer = defer;
};
var defer = function (dict) {
    return dict.defer;
};
var fix = function (dictLazy) {
    return function (f) {
        return defer(dictLazy)(function (v) {
            return f(fix(dictLazy)(f));
        });
    };
};
module.exports = {
    Lazy: Lazy, 
    fix: fix, 
    defer: defer
};

},{"../Prelude":203}],38:[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Aff.AVar

exports._makeVar = function (nonCanceler) {
  return function(success, error) {
    try {
      success({
        consumers: [],
        producers: [],
        error: undefined
      });
    } catch (err) {
      error(err);
    }

    return nonCanceler;
  }
}

exports._takeVar = function (nonCanceler, avar) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.producers.length > 0) {
      var producer = avar.producers.shift();

      producer(success, error);
    } else {
      avar.consumers.push({success: success, error: error});
    }

    return nonCanceler;
  }
}

exports._putVar = function (nonCanceler, avar, a) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.consumers.length === 0) {
      avar.producers.push(function(success, error) {
        try {
          success(a);
        } catch (err) {
          error(err);
        }
      });

      success({});
    } else {
      var consumer = avar.consumers.shift();

      try {
        consumer.success(a);
      } catch (err) {
        error(err);

        return;
      }

      success({});
    }

    return nonCanceler;
  }
}

exports._killVar = function (nonCanceler, avar, e) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else {
      var errors = [];

      avar.error = e;

      while (avar.consumers.length > 0) {
        var consumer = avar.consumers.shift();

        try {
          consumer.error(e);
        } catch (err) {
          errors.push(err);
        }
      }

      if (errors.length > 0) error(errors[0]);
      else success({});
    }

    return nonCanceler;
  }
}

},{}],39:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Data_Function = require("../Data.Function");
var takeVar = function (q) {
    return $foreign._takeVar(Control_Monad_Aff.nonCanceler, q);
};
var putVar = function (q) {
    return function (a) {
        return $foreign._putVar(Control_Monad_Aff.nonCanceler, q, a);
    };
};
var modifyVar = function (f) {
    return function (v) {
        return Prelude[">>="](Control_Monad_Aff.bindAff)(takeVar(v))(function ($2) {
            return putVar(v)(f($2));
        });
    };
};
var makeVar = $foreign._makeVar(Control_Monad_Aff.nonCanceler);
var makeVar$prime = function (a) {
    return Prelude.bind(Control_Monad_Aff.bindAff)(makeVar)(function (v) {
        return Prelude.bind(Control_Monad_Aff.bindAff)(putVar(v)(a))(function () {
            return Prelude["return"](Control_Monad_Aff.applicativeAff)(v);
        });
    });
};
var killVar = function (q) {
    return function (e) {
        return $foreign._killVar(Control_Monad_Aff.nonCanceler, q, e);
    };
};
module.exports = {
    takeVar: takeVar, 
    putVar: putVar, 
    modifyVar: modifyVar, 
    "makeVar'": makeVar$prime, 
    makeVar: makeVar, 
    killVar: killVar
};

},{"../Control.Monad.Aff":43,"../Control.Monad.Eff.Exception":48,"../Data.Function":130,"../Prelude":203,"./foreign":38}],40:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Cont_Trans = require("../Control.Monad.Cont.Trans");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Control_Monad_List_Trans = require("../Control.Monad.List.Trans");
var Control_Monad_Maybe_Trans = require("../Control.Monad.Maybe.Trans");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Monad_RWS_Trans = require("../Control.Monad.RWS.Trans");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans");
var Data_Monoid = require("../Data.Monoid");
var MonadAff = function (__superclass_Control$dotMonad$dotEff$dotClass$dotMonadEff_0, liftAff) {
    this["__superclass_Control.Monad.Eff.Class.MonadEff_0"] = __superclass_Control$dotMonad$dotEff$dotClass$dotMonadEff_0;
    this.liftAff = liftAff;
};
var monadAffAff = new MonadAff(function () {
    return Control_Monad_Aff.monadEffAff;
}, Prelude.id(Prelude.categoryFn));
var liftAff = function (dict) {
    return dict.liftAff;
};
var monadAffContT = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_Cont_Trans.monadEffContT(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($10) {
        return Control_Monad_Trans.lift(Control_Monad_Cont_Trans.monadTransContT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())(liftAff(dictMonadAff)($10));
    });
};
var monadAffExceptT = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_Except_Trans.monadEffExceptT(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($11) {
        return Control_Monad_Trans.lift(Control_Monad_Except_Trans.monadTransExceptT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())(liftAff(dictMonadAff)($11));
    });
};
var monadAffListT = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_List_Trans.monadEffListT(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($12) {
        return Control_Monad_Trans.lift(Control_Monad_List_Trans.monadTransListT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())(liftAff(dictMonadAff)($12));
    });
};
var monadAffMaybe = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_Maybe_Trans.monadEffMaybe(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($13) {
        return Control_Monad_Trans.lift(Control_Monad_Maybe_Trans.monadTransMaybeT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())(liftAff(dictMonadAff)($13));
    });
};
var monadAffRWS = function (dictMonadAff) {
    return function (dictMonoid) {
        return new MonadAff(function () {
            return Control_Monad_RWS_Trans.monadEffRWS((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())(dictMonoid)(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
        }, function ($14) {
            return Control_Monad_Trans.lift(Control_Monad_RWS_Trans.monadTransRWST(dictMonoid))((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())(liftAff(dictMonadAff)($14));
        });
    };
};
var monadAffReader = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_Reader_Trans.monadEffReader(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($15) {
        return Control_Monad_Trans.lift(Control_Monad_Reader_Trans.monadTransReaderT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())(liftAff(dictMonadAff)($15));
    });
};
var monadAffState = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_State_Trans.monadEffState((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($16) {
        return Control_Monad_Trans.lift(Control_Monad_State_Trans.monadTransStateT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())(liftAff(dictMonadAff)($16));
    });
};
var monadAffWriter = function (dictMonadAff) {
    return function (dictMonoid) {
        return new MonadAff(function () {
            return Control_Monad_Writer_Trans.monadEffWriter((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())(dictMonoid)(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
        }, function ($17) {
            return Control_Monad_Trans.lift(Control_Monad_Writer_Trans.monadTransWriterT(dictMonoid))((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Prelude.Monad_0"]())(liftAff(dictMonadAff)($17));
        });
    };
};
module.exports = {
    MonadAff: MonadAff, 
    liftAff: liftAff, 
    monadAffAff: monadAffAff, 
    monadAffContT: monadAffContT, 
    monadAffExceptT: monadAffExceptT, 
    monadAffListT: monadAffListT, 
    monadAffMaybe: monadAffMaybe, 
    monadAffReader: monadAffReader, 
    monadAffRWS: monadAffRWS, 
    monadAffState: monadAffState, 
    monadAffWriter: monadAffWriter
};

},{"../Control.Monad.Aff":43,"../Control.Monad.Cont.Trans":45,"../Control.Monad.Eff.Class":46,"../Control.Monad.Except.Trans":56,"../Control.Monad.List.Trans":59,"../Control.Monad.Maybe.Trans":60,"../Control.Monad.RWS.Trans":62,"../Control.Monad.Reader.Trans":64,"../Control.Monad.State.Trans":69,"../Control.Monad.Trans":71,"../Control.Monad.Writer.Trans":73,"../Data.Monoid":159,"../Prelude":203}],41:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Cont_Trans = require("../Control.Monad.Cont.Trans");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Control_Monad_Free = require("../Control.Monad.Free");
var Control_Monad_List_Trans = require("../Control.Monad.List.Trans");
var Control_Monad_Maybe_Trans = require("../Control.Monad.Maybe.Trans");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Monad_RWS_Trans = require("../Control.Monad.RWS.Trans");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans");
var Data_Monoid = require("../Data.Monoid");
var Affable = function (fromAff) {
    this.fromAff = fromAff;
};
var fromAff = function (dict) {
    return dict.fromAff;
};
var fromEff = function (dictAffable) {
    return function (eff) {
        return fromAff(dictAffable)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(eff));
    };
};
var monadAffContT = function (dictAffable) {
    return function (dictMonad) {
        return new Affable(function ($20) {
            return Control_Monad_Trans.lift(Control_Monad_Cont_Trans.monadTransContT)(dictMonad)(fromAff(dictAffable)($20));
        });
    };
};
var monadAffExceptT = function (dictAffable) {
    return function (dictMonad) {
        return new Affable(function ($21) {
            return Control_Monad_Trans.lift(Control_Monad_Except_Trans.monadTransExceptT)(dictMonad)(fromAff(dictAffable)($21));
        });
    };
};
var monadAffListT = function (dictAffable) {
    return function (dictMonad) {
        return new Affable(function ($22) {
            return Control_Monad_Trans.lift(Control_Monad_List_Trans.monadTransListT)(dictMonad)(fromAff(dictAffable)($22));
        });
    };
};
var monadAffMaybe = function (dictAffable) {
    return function (dictMonad) {
        return new Affable(function ($23) {
            return Control_Monad_Trans.lift(Control_Monad_Maybe_Trans.monadTransMaybeT)(dictMonad)(fromAff(dictAffable)($23));
        });
    };
};
var monadAffRWS = function (dictAffable) {
    return function (dictMonad) {
        return function (dictMonoid) {
            return new Affable(function ($24) {
                return Control_Monad_Trans.lift(Control_Monad_RWS_Trans.monadTransRWST(dictMonoid))(dictMonad)(fromAff(dictAffable)($24));
            });
        };
    };
};
var monadAffReader = function (dictAffable) {
    return function (dictMonad) {
        return new Affable(function ($25) {
            return Control_Monad_Trans.lift(Control_Monad_Reader_Trans.monadTransReaderT)(dictMonad)(fromAff(dictAffable)($25));
        });
    };
};
var monadAffState = function (dictAffable) {
    return function (dictMonad) {
        return new Affable(function ($26) {
            return Control_Monad_Trans.lift(Control_Monad_State_Trans.monadTransStateT)(dictMonad)(fromAff(dictAffable)($26));
        });
    };
};
var monadAffWriter = function (dictAffable) {
    return function (dictMonad) {
        return function (dictMonoid) {
            return new Affable(function ($27) {
                return Control_Monad_Trans.lift(Control_Monad_Writer_Trans.monadTransWriterT(dictMonoid))(dictMonad)(fromAff(dictAffable)($27));
            });
        };
    };
};
var affableFree = function (dictAffable) {
    return new Affable(function ($28) {
        return Control_Monad_Free.liftF(fromAff(dictAffable)($28));
    });
};
var affableAff = new Affable(Prelude.id(Prelude.categoryFn));
module.exports = {
    Affable: Affable, 
    fromEff: fromEff, 
    fromAff: fromAff, 
    affableAff: affableAff, 
    affableFree: affableFree, 
    monadAffContT: monadAffContT, 
    monadAffExceptT: monadAffExceptT, 
    monadAffListT: monadAffListT, 
    monadAffMaybe: monadAffMaybe, 
    monadAffReader: monadAffReader, 
    monadAffRWS: monadAffRWS, 
    monadAffState: monadAffState, 
    monadAffWriter: monadAffWriter
};

},{"../Control.Monad.Aff":43,"../Control.Monad.Cont.Trans":45,"../Control.Monad.Eff":54,"../Control.Monad.Eff.Class":46,"../Control.Monad.Except.Trans":56,"../Control.Monad.Free":58,"../Control.Monad.List.Trans":59,"../Control.Monad.Maybe.Trans":60,"../Control.Monad.RWS.Trans":62,"../Control.Monad.Reader.Trans":64,"../Control.Monad.State.Trans":69,"../Control.Monad.Trans":71,"../Control.Monad.Writer.Trans":73,"../Data.Monoid":159,"../Prelude":203}],42:[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Aff

exports._cancelWith = function (nonCanceler, aff, canceler1) {
  return function(success, error) {
    var canceler2 = aff(success, error);

    return function(e) {
      return function(success, error) {
        var cancellations = 0;
        var result        = false;
        var errored       = false;

        var s = function(bool) {
          cancellations = cancellations + 1;
          result        = result || bool;

          if (cancellations === 2 && !errored) {
            try {
              success(result);
            } catch (err) {
              error(err);
            }
          }
        };

        var f = function(err) {
          if (!errored) {
            errored = true;

            error(err);
          }
        };

        canceler2(e)(s, f);
        canceler1(e)(s, f);

        return nonCanceler;
      };
    };
  };
}

exports._setTimeout = function (nonCanceler, millis, aff) {
  var set = setTimeout, clear = clearTimeout;
  if (millis <= 0 && typeof setImmediate === "function") {
    set = setImmediate;
    clear = clearImmediate;
  }
  return function(success, error) {
    var canceler;

    var timeout = set(function() {
      canceler = aff(success, error);
    }, millis);

    return function(e) {
      return function(s, f) {
        if (canceler !== undefined) {
          return canceler(e)(s, f);
        } else {
          clear(timeout);

          try {
            s(true);
          } catch (err) {
            f(err);
          }

          return nonCanceler;
        }
      };
    };
  };
}

exports._unsafeInterleaveAff = function (aff) {
  return aff;
}

exports._forkAff = function (nonCanceler, aff) {
  var voidF = function(){};

  return function(success, error) {
    var canceler = aff(voidF, voidF);

    try {
      success(canceler);
    } catch (err) {
      error(err);
    }

    return nonCanceler;
  };
}

exports._forkAll = function (nonCanceler, foldl, affs) {
  var voidF = function(){};

  return function(success, error) {
    var cancelers = foldl(function(acc) {
      return function(aff) {
        acc.push(aff(voidF, voidF));
        return acc;
      }
    })([])(affs);

    var canceler = function(e) {
      return function(success, error) {
        var cancellations = 0;
        var result        = false;
        var errored       = false;

        var s = function(bool) {
          cancellations = cancellations + 1;
          result        = result || bool;

          if (cancellations === cancelers.length && !errored) {
            try {
              success(result);
            } catch (err) {
              error(err);
            }
          }
        };

        var f = function(err) {
          if (!errored) {
            errored = true;
            error(err);
          }
        };

        for (var i = 0; i < cancelers.length; i++) {
          cancelers[i](e)(s, f);
        }

        return nonCanceler;
      };
    };

    try {
      success(canceler);
    } catch (err) {
      error(err);
    }

    return nonCanceler;
  };
}

exports._makeAff = function (cb) {
  return function(success, error) {
    return cb(function(e) {
      return function() {
        error(e);
      };
    })(function(v) {
      return function() {
        try {
          success(v);
        } catch (err) {
          error(err);
        }
      };
    })();
  }
}

exports._pure = function (nonCanceler, v) {
  return function(success, error) {
    try {
      success(v);
    } catch (err) {
      error(err);
    }

    return nonCanceler;
  };
}

exports._throwError = function (nonCanceler, e) {
  return function(success, error) {
    error(e);

    return nonCanceler;
  };
}

exports._fmap = function (f, aff) {
  return function(success, error) {
    return aff(function(v) {
      try {
        success(f(v));
      } catch (err) {
        error(err);
      }
    }, error);
  };
}

exports._bind = function (alwaysCanceler, aff, f) {
  return function(success, error) {
    var canceler1, canceler2;

    var isCanceled    = false;
    var requestCancel = false;

    var onCanceler = function(){};

    canceler1 = aff(function(v) {
      if (requestCancel) {
        isCanceled = true;

        return alwaysCanceler;
      } else {
        canceler2 = f(v)(success, error);

        onCanceler(canceler2);

        return canceler2;
      }
    }, error);

    return function(e) {
      return function(s, f) {
        requestCancel = true;

        if (canceler2 !== undefined) {
          return canceler2(e)(s, f);
        } else {
          return canceler1(e)(function(bool) {
            if (bool || isCanceled) {
              try {
                s(true);
              } catch (err) {
                f(err);
              }
            } else {
              onCanceler = function(canceler) {
                canceler(e)(s, f);
              };
            }
          }, f);
        }
      };
    };
  };
}

exports._attempt = function (Left, Right, aff) {
  return function(success, error) {
    return aff(function(v) {
      try {
        success(Right(v));
      } catch (err) {
        error(err);
      }
    }, function(e) {
      try {
        success(Left(e));
      } catch (err) {
        error(err);
      }
    });
  };
}

exports._runAff = function (errorT, successT, aff) {
  return function() {
    return aff(function(v) {
      try {
        successT(v)();
      } catch (err) {
        errorT(err)();
      }
    }, function(e) {
      errorT(e)();
    });
  };
}

exports._liftEff = function (nonCanceler, e) {
  return function(success, error) {
    try {
      success(e());
    } catch (err) {
      error(err);
    }

    return nonCanceler;
  };
}

exports._tailRecM = function (isLeft, f, a) {
  return function(success, error) {
    return function go(acc) {
      var result, status, canceler;

      // Observes synchronous effects using a flag.
      //   status = 0 (unresolved status)
      //   status = 1 (synchronous effect)
      //   status = 2 (asynchronous effect)
      while (true) {
        status = 0;
        canceler = f(acc)(function(v) {
          // If the status is still unresolved, we have observed a
          // synchronous effect. Otherwise, the status will be `2`.
          if (status === 0) {
            // Store the result for further synchronous processing.
            result = v;
            status = 1;
          } else {
            // When we have observed an asynchronous effect, we use normal
            // recursion. This is safe because we will be on a new stack.
            if (isLeft(v)) {
              go(v.value0);
            } else {
              try {
                success(v.value0);
              } catch (err) {
                error(err);
              }
            }
          }
        }, error);

        // If the status has already resolved to `1` by our Aff handler, then
        // we have observed a synchronous effect. Otherwise it will still be
        // `0`.
        if (status === 1) {
          // When we have observed a synchronous effect, we merely swap out the
          // accumulator and continue the loop, preserving stack.
          if (isLeft(result)) {
            acc = result.value0;
            continue;
          } else {
            try {
              success(result.value0);
            } catch (err) {
              error(err);
            }
          }
        } else {
          // If the status has not resolved yet, then we have observed an
          // asynchronous effect.
          status = 2;
        }
        return canceler;
      }

    }(a);
  };
};

},{}],43:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_Plus = require("../Control.Plus");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Monoid = require("../Data.Monoid");
var Canceler = function (x) {
    return x;
};
var runAff = function (ex) {
    return function (f) {
        return function (aff) {
            return $foreign._runAff(ex, f, aff);
        };
    };
};
var makeAff$prime = function (h) {
    return $foreign._makeAff(h);
};
var launchAff = function ($15) {
    return runAff(Control_Monad_Eff_Exception.throwException)(Prelude["const"](Prelude.pure(Control_Monad_Eff.applicativeEff)(Prelude.unit)))($foreign._unsafeInterleaveAff($15));
};
var functorAff = new Prelude.Functor(function (f) {
    return function (fa) {
        return $foreign._fmap(f, fa);
    };
});
var cancel = function (v) {
    return v;
};
var attempt = function (aff) {
    return $foreign._attempt(Data_Either.Left.create, Data_Either.Right.create, aff);
};
var apathize = function (a) {
    return Prelude["<$>"](functorAff)(Prelude["const"](Prelude.unit))(attempt(a));
};
var applyAff = new Prelude.Apply(function () {
    return functorAff;
}, function (ff) {
    return function (fa) {
        return $foreign._bind(alwaysCanceler, ff, function (f) {
            return Prelude["<$>"](functorAff)(f)(fa);
        });
    };
});
var applicativeAff = new Prelude.Applicative(function () {
    return applyAff;
}, function (v) {
    return $foreign._pure(nonCanceler, v);
});
var nonCanceler = Prelude["const"](Prelude.pure(applicativeAff)(false));
var alwaysCanceler = Prelude["const"](Prelude.pure(applicativeAff)(true));
var cancelWith = function (aff) {
    return function (c) {
        return $foreign._cancelWith(nonCanceler, aff, c);
    };
};
var forkAff = function (aff) {
    return $foreign._forkAff(nonCanceler, aff);
};
var forkAll = function (dictFoldable) {
    return function (affs) {
        return $foreign._forkAll(nonCanceler, Data_Foldable.foldl(dictFoldable), affs);
    };
};
var later$prime = function (n) {
    return function (aff) {
        return $foreign._setTimeout(nonCanceler, n, aff);
    };
};
var later = later$prime(0);
var liftEff$prime = function (eff) {
    return attempt($foreign._unsafeInterleaveAff($foreign._liftEff(nonCanceler, eff)));
};
var makeAff = function (h) {
    return makeAff$prime(function (e) {
        return function (a) {
            return Prelude["<$>"](Control_Monad_Eff.functorEff)(Prelude["const"](nonCanceler))(h(e)(a));
        };
    });
};
var semigroupAff = function (dictSemigroup) {
    return new Prelude.Semigroup(function (a) {
        return function (b) {
            return Prelude["<*>"](applyAff)(Prelude["<$>"](functorAff)(Prelude["<>"](dictSemigroup))(a))(b);
        };
    });
};
var monoidAff = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupAff(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Prelude.pure(applicativeAff)(Data_Monoid.mempty(dictMonoid)));
};
var semigroupCanceler = new Prelude.Semigroup(function (v) {
    return function (v1) {
        return function (e) {
            return Prelude["<*>"](applyAff)(Prelude["<$>"](functorAff)(Prelude["||"](Prelude.booleanAlgebraBoolean))(v(e)))(v1(e));
        };
    };
});
var monoidCanceler = new Data_Monoid.Monoid(function () {
    return semigroupCanceler;
}, Prelude["const"](Prelude.pure(applicativeAff)(true)));
var bindAff = new Prelude.Bind(function () {
    return applyAff;
}, function (fa) {
    return function (f) {
        return $foreign._bind(alwaysCanceler, fa, f);
    };
});
var monadAff = new Prelude.Monad(function () {
    return applicativeAff;
}, function () {
    return bindAff;
});
var monadContAff = new Control_Monad_Cont_Class.MonadCont(function () {
    return monadAff;
}, function (f) {
    return makeAff(function (eb) {
        return function (cb) {
            return runAff(eb)(cb)(f(function (a) {
                return makeAff(function (v) {
                    return function (v1) {
                        return cb(a);
                    };
                });
            }));
        };
    });
});
var monadEffAff = new Control_Monad_Eff_Class.MonadEff(function () {
    return monadAff;
}, function (eff) {
    return $foreign._liftEff(nonCanceler, eff);
});
var monadRecAff = new Control_Monad_Rec_Class.MonadRec(function () {
    return monadAff;
}, function (f) {
    return function (a) {
        return $foreign._tailRecM(Data_Either.isLeft, f, a);
    };
});
var monadErrorAff = new Control_Monad_Error_Class.MonadError(function () {
    return monadAff;
}, function (aff) {
    return function (ex) {
        return Prelude[">>="](bindAff)(attempt(aff))(Data_Either.either(ex)(Prelude.pure(applicativeAff)));
    };
}, function (e) {
    return $foreign._throwError(nonCanceler, e);
});
var $$finally = function (aff1) {
    return function (aff2) {
        return Prelude.bind(bindAff)(attempt(aff1))(function (v) {
            return Prelude.bind(bindAff)(aff2)(function () {
                return Data_Either.either(Control_Monad_Error_Class.throwError(monadErrorAff))(Prelude.pure(applicativeAff))(v);
            });
        });
    };
};
var altAff = new Control_Alt.Alt(function () {
    return functorAff;
}, function (a1) {
    return function (a2) {
        return Prelude[">>="](bindAff)(attempt(a1))(Data_Either.either(Prelude["const"](a2))(Prelude.pure(applicativeAff)));
    };
});
var plusAff = new Control_Plus.Plus(function () {
    return altAff;
}, Control_Monad_Error_Class.throwError(monadErrorAff)(Control_Monad_Eff_Exception.error("Always fails")));
var alternativeAff = new Control_Alternative.Alternative(function () {
    return plusAff;
}, function () {
    return applicativeAff;
});
var monadPlusAff = new Control_MonadPlus.MonadPlus(function () {
    return alternativeAff;
}, function () {
    return monadAff;
});
module.exports = {
    Canceler: Canceler, 
    runAff: runAff, 
    nonCanceler: nonCanceler, 
    "makeAff'": makeAff$prime, 
    makeAff: makeAff, 
    "liftEff'": liftEff$prime, 
    launchAff: launchAff, 
    "later'": later$prime, 
    later: later, 
    forkAll: forkAll, 
    forkAff: forkAff, 
    "finally": $$finally, 
    cancelWith: cancelWith, 
    cancel: cancel, 
    attempt: attempt, 
    apathize: apathize, 
    semigroupAff: semigroupAff, 
    monoidAff: monoidAff, 
    functorAff: functorAff, 
    applyAff: applyAff, 
    applicativeAff: applicativeAff, 
    bindAff: bindAff, 
    monadAff: monadAff, 
    monadEffAff: monadEffAff, 
    monadErrorAff: monadErrorAff, 
    altAff: altAff, 
    plusAff: plusAff, 
    alternativeAff: alternativeAff, 
    monadPlusAff: monadPlusAff, 
    monadRecAff: monadRecAff, 
    monadContAff: monadContAff, 
    semigroupCanceler: semigroupCanceler, 
    monoidCanceler: monoidCanceler
};

},{"../Control.Alt":26,"../Control.Alternative":27,"../Control.Monad.Cont.Class":44,"../Control.Monad.Eff":54,"../Control.Monad.Eff.Class":46,"../Control.Monad.Eff.Exception":48,"../Control.Monad.Error.Class":55,"../Control.Monad.Rec.Class":65,"../Control.MonadPlus":76,"../Control.Plus":77,"../Data.Either":114,"../Data.Foldable":120,"../Data.Function":130,"../Data.Monoid":159,"../Prelude":203,"./foreign":42}],44:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var MonadCont = function (__superclass_Prelude$dotMonad_0, callCC) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.callCC = callCC;
};
var callCC = function (dict) {
    return dict.callCC;
};
module.exports = {
    MonadCont: MonadCont, 
    callCC: callCC
};

},{"../Prelude":203}],45:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var ContT = function (x) {
    return x;
};
var runContT = function (v) {
    return function (k) {
        return v(k);
    };
};
var withContT = function (f) {
    return function (m) {
        return function (k) {
            return runContT(m)(f(k));
        };
    };
};
var monadTransContT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function (m) {
        return function (k) {
            return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(m)(k);
        };
    };
});
var mapContT = function (f) {
    return function (m) {
        return function (k) {
            return f(runContT(m)(k));
        };
    };
};
var functorContT = function (dictMonad) {
    return new Prelude.Functor(function (f) {
        return function (m) {
            return function (k) {
                return runContT(m)(function (a) {
                    return k(f(a));
                });
            };
        };
    });
};
var applyContT = function (dictMonad) {
    return new Prelude.Apply(function () {
        return functorContT(dictMonad);
    }, function (f) {
        return function (v) {
            return function (k) {
                return runContT(f)(function (g) {
                    return runContT(v)(function (a) {
                        return k(g(a));
                    });
                });
            };
        };
    });
};
var bindContT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyContT(dictMonad);
    }, function (m) {
        return function (k) {
            return function (k$prime) {
                return runContT(m)(function (a) {
                    return runContT(k(a))(k$prime);
                });
            };
        };
    });
};
var applicativeContT = function (dictMonad) {
    return new Prelude.Applicative(function () {
        return applyContT(dictMonad);
    }, function (a) {
        return function (k) {
            return k(a);
        };
    });
};
var monadContT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeContT(dictMonad);
    }, function () {
        return bindContT(dictMonad);
    });
};
var monadContContT = function (dictMonad) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadContT(dictMonad);
    }, function (f) {
        return function (k) {
            return runContT(f(function (a) {
                return function (v) {
                    return k(a);
                };
            }))(k);
        };
    });
};
var monadEffContT = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadContT(dictMonadEff["__superclass_Prelude.Monad_0"]());
    }, function ($17) {
        return Control_Monad_Trans.lift(monadTransContT)(dictMonadEff["__superclass_Prelude.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($17));
    });
};
var monadReaderContT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadContT(dictMonadReader["__superclass_Prelude.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransContT)(dictMonadReader["__superclass_Prelude.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return function (c) {
            return function (k) {
                return Prelude.bind((dictMonadReader["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(Control_Monad_Reader_Class.ask(dictMonadReader))(function (v) {
                    return Control_Monad_Reader_Class.local(dictMonadReader)(f)(runContT(c)(function ($18) {
                        return Control_Monad_Reader_Class.local(dictMonadReader)(Prelude["const"](v))(k($18));
                    }));
                });
            };
        };
    });
};
var monadStateContT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadContT(dictMonadState["__superclass_Prelude.Monad_0"]());
    }, function ($19) {
        return Control_Monad_Trans.lift(monadTransContT)(dictMonadState["__superclass_Prelude.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)($19));
    });
};
module.exports = {
    ContT: ContT, 
    withContT: withContT, 
    mapContT: mapContT, 
    runContT: runContT, 
    monadContContT: monadContContT, 
    functorContT: functorContT, 
    applyContT: applyContT, 
    applicativeContT: applicativeContT, 
    bindContT: bindContT, 
    monadContT: monadContT, 
    monadTransContT: monadTransContT, 
    monadEffContT: monadEffContT, 
    monadReaderContT: monadReaderContT, 
    monadStateContT: monadStateContT
};

},{"../Control.Monad.Cont.Class":44,"../Control.Monad.Eff.Class":46,"../Control.Monad.Reader.Class":63,"../Control.Monad.State.Class":68,"../Control.Monad.Trans":71,"../Prelude":203}],46:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var MonadEff = function (__superclass_Prelude$dotMonad_0, liftEff) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.liftEff = liftEff;
};
var monadEffEff = new MonadEff(function () {
    return Control_Monad_Eff.monadEff;
}, Prelude.id(Prelude.categoryFn));
var liftEff = function (dict) {
    return dict.liftEff;
};
module.exports = {
    MonadEff: MonadEff, 
    liftEff: liftEff, 
    monadEffEff: monadEffEff
};

},{"../Control.Monad.Eff":54,"../Prelude":203}],47:[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Eff.Exception

exports.showErrorImpl = function (err) {
  return err.stack || err.toString();
};

exports.error = function (msg) {
  return new Error(msg);
};

exports.message = function (e) {
  return e.message;
};

exports.stackImpl = function (just) {
  return function (nothing) {
    return function (e) {
      return e.stack ? just(e.stack) : nothing;
    };
  };
};

exports.throwException = function (e) {
  return function () {
    throw e;
  };
};

exports.catchException = function (c) {
  return function (t) {
    return function () {
      try {
        return t();
      } catch (e) {
        if (e instanceof Error || Object.prototype.toString.call(e) === "[object Error]") {
          return c(e)();
        } else {
          return c(new Error(e.toString()))();
        }
      }
    };
  };
};

},{}],48:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var $$throw = function ($0) {
    return $foreign.throwException($foreign.error($0));
};
var stack = $foreign.stackImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var showError = new Prelude.Show($foreign.showErrorImpl);
module.exports = {
    "throw": $$throw, 
    stack: stack, 
    showError: showError, 
    catchException: $foreign.catchException, 
    throwException: $foreign.throwException, 
    message: $foreign.message, 
    error: $foreign.error
};

},{"../Control.Monad.Eff":54,"../Data.Maybe":152,"../Prelude":203,"./foreign":47}],49:[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Eff.JQuery

exports.ready = function(func) {
    return function() {
        jQuery(document).ready(func);
    };
};

exports.select = function(selector) {
    return function() {
        return jQuery(selector);
    };
};

exports.find = function(selector) {
    return function(ob) {
        return function() {
            return ob.find(selector);
        };
    };
};

exports.parent = function(ob) {
    return function() {
        return ob.parent();
    };
};

exports.closest = function(selector) {
    return function(ob) {
        return function() {
            return ob.closest(selector);
        };
    };
};

exports.create = function(html) {
    return function() {
        return jQuery(html);
    };
};

exports.setAttr = function(attr) {
    return function(val) {
        return function(ob) {
            return function() {
                return ob.attr(attr, val);
            };
        };
    };
};

exports.attr = function(attrs) {
    return function(ob) {
        return function() {
            return ob.attr(attrs);
        };
    };
};

exports.css = function(props) {
    return function(ob) {
        return function() {
            return ob.css(props);
        };
    };
};

exports.hasClass = function(cls) {
    return function(ob) {
        return function() {
            return ob.hasClass(cls);
        };
    };
};

exports.toggleClass = function(cls) {
    return function(ob) {
        return function() {
            return ob.toggleClass(cls);
        };
    };
};

exports.setClass = function(cls) {
    return function(flag) {
        return function(ob) {
            return function() {
                return ob.toggleClass(cls, flag);
            };
        };
    };
};

exports.setProp = function(p) {
    return function(val) {
        return function(ob) {
            return function() {
                return ob.prop(p, val);
            };
        };
    };
};

exports.getProp = function(p) {
    return function(ob) {
        return function() {
            return ob.prop(p);
        };
    };
};

exports.append = function(ob1) {
    return function(ob) {
        return function() {
            return ob.append(ob1);
        };
    };
};

exports.appendText = function(s) {
    return function(ob) {
        return function() {
            return ob.append(s);
        };
    };
};

exports.body = function() {
    return jQuery(document.body);
};

exports.remove = function(ob) {
    return function() {
        return ob.remove();
    };
};

exports.clear = function(ob) {
    return function() {
        return ob.empty();
    };
};

exports.before = function(ob) {
    return function(ob1) {
        return function() {
            return ob1.before(ob);
        };
    };
};

exports.getText = function(ob) {
    return function() {
        return ob.text();
    };
};

exports.setText = function(text) {
    return function(ob) {
        return function() {
            return ob.text(text);
        };
    };
};

exports.getValue = function(ob) {
    return function() {
        return ob.val();
    };
};

exports.setValue = function(val) {
    return function(ob) {
        return function() {
            return ob.val(val);
        };
    };
};

exports.toggle = function(ob) {
    return function() {
        return ob.toggle();
    };
};

exports.setVisible = function(flag) {
    return function(ob) {
        return function() {
            return ob.toggle(flag);
        };
    };
};

exports.on = function(evt) {
    return function(act) {
        return function(ob) {
            return function() {
                return ob.on(evt, function(e) {
                    act(e)(jQuery(this))();
                });
            };
        };
    };
};

exports["on'"] = function(evt) {
    return function(sel) {
        return function(act) {
            return function(ob) {
                return function() {
                    return ob.on(evt, sel, function(e) {
                        act(e)(jQuery(this))();
                    });
                };
            };
        };
    };
};

exports.preventDefault = function(e) {
    return function() {
        e.preventDefault();
    };
};

exports.stopPropagation = function(e) {
    return function() {
        e.stopPropagation();
    };
};

exports.stopImmediatePropagation = function(e) {
    return function() {
        e.stopImmediatePropagation();
    };
};

},{}],50:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var DOM = require("../DOM");
var Data_Foreign = require("../Data.Foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var removeClass = function (cls) {
    return $foreign.setClass(cls)(false);
};
var hide = $foreign.setVisible(false);
var display = $foreign.setVisible(true);
var addClass = function (cls) {
    return $foreign.setClass(cls)(true);
};
module.exports = {
    display: display, 
    hide: hide, 
    removeClass: removeClass, 
    addClass: addClass, 
    stopImmediatePropagation: $foreign.stopImmediatePropagation, 
    stopPropagation: $foreign.stopPropagation, 
    preventDefault: $foreign.preventDefault, 
    "on'": $foreign["on'"], 
    on: $foreign.on, 
    setVisible: $foreign.setVisible, 
    toggle: $foreign.toggle, 
    setValue: $foreign.setValue, 
    getValue: $foreign.getValue, 
    setText: $foreign.setText, 
    getText: $foreign.getText, 
    body: $foreign.body, 
    appendText: $foreign.appendText, 
    before: $foreign.before, 
    clear: $foreign.clear, 
    remove: $foreign.remove, 
    append: $foreign.append, 
    getProp: $foreign.getProp, 
    setProp: $foreign.setProp, 
    setClass: $foreign.setClass, 
    toggleClass: $foreign.toggleClass, 
    hasClass: $foreign.hasClass, 
    css: $foreign.css, 
    attr: $foreign.attr, 
    setAttr: $foreign.setAttr, 
    create: $foreign.create, 
    closest: $foreign.closest, 
    parent: $foreign.parent, 
    find: $foreign.find, 
    select: $foreign.select, 
    ready: $foreign.ready
};

},{"../Control.Monad.Eff":54,"../DOM":98,"../Data.Foreign":128,"../Prelude":203,"./foreign":49}],51:[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Eff.Unsafe

exports.unsafeInterleaveEff = function (f) {
  return f;
};

},{}],52:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var unsafePerformEff = function ($0) {
    return Control_Monad_Eff.runPure($foreign.unsafeInterleaveEff($0));
};
module.exports = {
    unsafePerformEff: unsafePerformEff, 
    unsafeInterleaveEff: $foreign.unsafeInterleaveEff
};

},{"../Control.Monad.Eff":54,"../Prelude":203,"./foreign":51}],53:[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Eff

exports.returnE = function (a) {
  return function () {
    return a;
  };
};

exports.bindE = function (a) {
  return function (f) {
    return function () {
      return f(a())();
    };
  };
};

exports.runPure = function (f) {
  return f();
};

exports.untilE = function (f) {
  return function () {
    while (!f());
    return {};
  };
};

exports.whileE = function (f) {
  return function (a) {
    return function () {
      while (f()) {
        a();
      }
      return {};
    };
  };
};

exports.forE = function (lo) {
  return function (hi) {
    return function (f) {
      return function () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
};

exports.foreachE = function (as) {
  return function (f) {
    return function () {
      for (var i = 0, l = as.length; i < l; i++) {
        f(as[i])();
      }
    };
  };
};

},{}],54:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var monadEff = new Prelude.Monad(function () {
    return applicativeEff;
}, function () {
    return bindEff;
});
var bindEff = new Prelude.Bind(function () {
    return applyEff;
}, $foreign.bindE);
var applyEff = new Prelude.Apply(function () {
    return functorEff;
}, Prelude.ap(monadEff));
var applicativeEff = new Prelude.Applicative(function () {
    return applyEff;
}, $foreign.returnE);
var functorEff = new Prelude.Functor(Prelude.liftA1(applicativeEff));
module.exports = {
    functorEff: functorEff, 
    applyEff: applyEff, 
    applicativeEff: applicativeEff, 
    bindEff: bindEff, 
    monadEff: monadEff, 
    foreachE: $foreign.foreachE, 
    forE: $foreign.forE, 
    whileE: $foreign.whileE, 
    untilE: $foreign.untilE, 
    runPure: $foreign.runPure
};

},{"../Prelude":203,"./foreign":53}],55:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Either = require("../Data.Either");
var MonadError = function (__superclass_Prelude$dotMonad_0, catchError, throwError) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.catchError = catchError;
    this.throwError = throwError;
};
var throwError = function (dict) {
    return dict.throwError;
};
var monadErrorMaybe = new MonadError(function () {
    return Data_Maybe.monadMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Data_Maybe.Nothing) {
            return v1(Prelude.unit);
        };
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(v.value0);
        };
        throw new Error("Failed pattern match at Control.Monad.Error.Class line 53, column 3 - line 54, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
}, Prelude["const"](Data_Maybe.Nothing.value));
var monadErrorEither = new MonadError(function () {
    return Data_Either.monadEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Data_Either.Left) {
            return v1(v.value0);
        };
        if (v instanceof Data_Either.Right) {
            return new Data_Either.Right(v.value0);
        };
        throw new Error("Failed pattern match at Control.Monad.Error.Class line 48, column 3 - line 49, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
}, Data_Either.Left.create);
var catchError = function (dict) {
    return dict.catchError;
};
var catchJust = function (dictMonadError) {
    return function (p) {
        return function (act) {
            return function (handler) {
                var handle = function (e) {
                    var $12 = p(e);
                    if ($12 instanceof Data_Maybe.Nothing) {
                        return throwError(dictMonadError)(e);
                    };
                    if ($12 instanceof Data_Maybe.Just) {
                        return handler($12.value0);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Error.Class line 42, column 5 - line 46, column 1: " + [ $12.constructor.name ]);
                };
                return catchError(dictMonadError)(act)(handle);
            };
        };
    };
};
module.exports = {
    MonadError: MonadError, 
    catchJust: catchJust, 
    catchError: catchError, 
    throwError: throwError, 
    monadErrorEither: monadErrorEither, 
    monadErrorMaybe: monadErrorMaybe
};

},{"../Data.Either":114,"../Data.Maybe":152,"../Prelude":203}],56:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Tuple = require("../Data.Tuple");
var Data_Either = require("../Data.Either");
var Data_Monoid = require("../Data.Monoid");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_RWS_Class = require("../Control.Monad.RWS.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_Plus = require("../Control.Plus");
var ExceptT = function (x) {
    return x;
};
var runExceptT = function (v) {
    return v;
};
var withExceptT = function (dictFunctor) {
    return function (f) {
        var mapLeft = function (v) {
            return function (v1) {
                if (v1 instanceof Data_Either.Right) {
                    return new Data_Either.Right(v1.value0);
                };
                if (v1 instanceof Data_Either.Left) {
                    return new Data_Either.Left(v(v1.value0));
                };
                throw new Error("Failed pattern match at Control.Monad.Except.Trans line 43, column 3 - line 44, column 3: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return function ($67) {
            return ExceptT(Prelude["<$>"](dictFunctor)(mapLeft(f))(runExceptT($67)));
        };
    };
};
var monadTransExceptT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function (m) {
        return ExceptT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
            return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(v));
        }));
    };
});
var mapExceptT = function (f) {
    return function (m) {
        return f(runExceptT(m));
    };
};
var functorExceptT = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        return mapExceptT(Prelude["<$>"](dictFunctor)(Prelude["<$>"](Data_Either.functorEither)(f)));
    });
};
var applyExceptT = function (dictApply) {
    return new Prelude.Apply(function () {
        return functorExceptT(dictApply["__superclass_Prelude.Functor_0"]());
    }, function (v) {
        return function (v1) {
            var f$prime = Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Prelude["<*>"](Data_Either.applyEither))(v);
            var x$prime = Prelude["<*>"](dictApply)(f$prime)(v1);
            return x$prime;
        };
    });
};
var bindExceptT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyExceptT((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]());
    }, function (m) {
        return function (k) {
            return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runExceptT(m))(Data_Either.either(function ($68) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Either.Left.create($68));
            })(function ($69) {
                return runExceptT(k($69));
            }));
        };
    });
};
var applicativeExceptT = function (dictApplicative) {
    return new Prelude.Applicative(function () {
        return applyExceptT(dictApplicative["__superclass_Prelude.Apply_0"]());
    }, function ($70) {
        return ExceptT(Prelude.pure(dictApplicative)(Data_Either.Right.create($70)));
    });
};
var monadExceptT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeExceptT(dictMonad["__superclass_Prelude.Applicative_0"]());
    }, function () {
        return bindExceptT(dictMonad);
    });
};
var monadContExceptT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadExceptT(dictMonadCont["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return ExceptT(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
            return runExceptT(f(function (a) {
                return ExceptT(c(new Data_Either.Right(a)));
            }));
        }));
    });
};
var monadEffExceptT = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadExceptT(dictMonadEff["__superclass_Prelude.Monad_0"]());
    }, function ($71) {
        return Control_Monad_Trans.lift(monadTransExceptT)(dictMonadEff["__superclass_Prelude.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($71));
    });
};
var monadErrorExceptT = function (dictMonad) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadExceptT(dictMonad);
    }, function (m) {
        return function (handler) {
            return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runExceptT(m))(Data_Either.either(function ($72) {
                return runExceptT(handler($72));
            })(function ($73) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Either.Right.create($73));
            }));
        };
    }, function ($74) {
        return ExceptT(Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Either.Left.create($74)));
    });
};
var monadReaderExceptT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadExceptT(dictMonadReader["__superclass_Prelude.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransExceptT)(dictMonadReader["__superclass_Prelude.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return mapExceptT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
    });
};
var monadRecExceptT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadExceptT(dictMonadRec["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return function ($75) {
            return ExceptT(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(function (a) {
                return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runExceptT(f(a)))(function (v) {
                    return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                        if (v instanceof Data_Either.Left) {
                            return new Data_Either.Right(new Data_Either.Left(v.value0));
                        };
                        if (v instanceof Data_Either.Right && v.value0 instanceof Data_Either.Left) {
                            return new Data_Either.Left(v.value0.value0);
                        };
                        if (v instanceof Data_Either.Right && v.value0 instanceof Data_Either.Right) {
                            return new Data_Either.Right(new Data_Either.Right(v.value0.value0));
                        };
                        throw new Error("Failed pattern match at Control.Monad.Except.Trans line 71, column 5 - line 76, column 1: " + [ v.constructor.name ]);
                    })());
                });
            })($75));
        };
    });
};
var monadStateExceptT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadExceptT(dictMonadState["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return Control_Monad_Trans.lift(monadTransExceptT)(dictMonadState["__superclass_Prelude.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
};
var monadWriterExceptT = function (dictMonadWriter) {
    return new Control_Monad_Writer_Class.MonadWriter(function () {
        return monadExceptT(dictMonadWriter["__superclass_Prelude.Monad_0"]());
    }, mapExceptT(function (m) {
        return Prelude.bind((dictMonadWriter["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m))(function (v) {
            return Prelude["return"]((dictMonadWriter["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(Prelude["<$>"](Data_Either.functorEither)(function (r) {
                return new Data_Tuple.Tuple(r, v.value1);
            })(v.value0));
        });
    }), mapExceptT(function (m) {
        return Control_Monad_Writer_Class.pass(dictMonadWriter)(Prelude.bind((dictMonadWriter["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(m)(function (v) {
            return Prelude["return"]((dictMonadWriter["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                if (v instanceof Data_Either.Left) {
                    return new Data_Tuple.Tuple(new Data_Either.Left(v.value0), Prelude.id(Prelude.categoryFn));
                };
                if (v instanceof Data_Either.Right) {
                    return new Data_Tuple.Tuple(new Data_Either.Right(v.value0.value0), v.value0.value1);
                };
                throw new Error("Failed pattern match at Control.Monad.Except.Trans line 123, column 5 - line 127, column 1: " + [ v.constructor.name ]);
            })());
        }));
    }), function (wd) {
        return Control_Monad_Trans.lift(monadTransExceptT)(dictMonadWriter["__superclass_Prelude.Monad_0"]())(Control_Monad_Writer_Class.writer(dictMonadWriter)(wd));
    });
};
var monadRWSExceptT = function (dictMonoid) {
    return function (dictMonadRWS) {
        return new Control_Monad_RWS_Class.MonadRWS(function () {
            return monadReaderExceptT(dictMonadRWS["__superclass_Control.Monad.Reader.Class.MonadReader_1"]());
        }, function () {
            return monadStateExceptT(dictMonadRWS["__superclass_Control.Monad.State.Class.MonadState_3"]());
        }, function () {
            return monadWriterExceptT(dictMonadRWS["__superclass_Control.Monad.Writer.Class.MonadWriter_2"]());
        }, function () {
            return dictMonoid;
        });
    };
};
var altExceptT = function (dictSemigroup) {
    return function (dictMonad) {
        return new Control_Alt.Alt(function () {
            return functorExceptT(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
        }, function (m) {
            return function (n) {
                return ExceptT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runExceptT(m))(function (v) {
                    if (v instanceof Data_Either.Right) {
                        return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(v.value0));
                    };
                    if (v instanceof Data_Either.Left) {
                        return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runExceptT(n))(function (v1) {
                            if (v1 instanceof Data_Either.Right) {
                                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(v1.value0));
                            };
                            if (v1 instanceof Data_Either.Left) {
                                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left(Prelude["<>"](dictSemigroup)(v.value0)(v1.value0)));
                            };
                            throw new Error("Failed pattern match at Control.Monad.Except.Trans line 83, column 9 - line 87, column 1: " + [ v1.constructor.name ]);
                        });
                    };
                    throw new Error("Failed pattern match at Control.Monad.Except.Trans line 79, column 5 - line 87, column 1: " + [ v.constructor.name ]);
                }));
            };
        });
    };
};
var plusExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Plus.Plus(function () {
            return altExceptT(dictMonoid["__superclass_Prelude.Semigroup_0"]())(dictMonad);
        }, Control_Monad_Error_Class.throwError(monadErrorExceptT(dictMonad))(Data_Monoid.mempty(dictMonoid)));
    };
};
var alternativeExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Alternative.Alternative(function () {
            return plusExceptT(dictMonoid)(dictMonad);
        }, function () {
            return applicativeExceptT(dictMonad["__superclass_Prelude.Applicative_0"]());
        });
    };
};
var monadPlusExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_MonadPlus.MonadPlus(function () {
            return alternativeExceptT(dictMonoid)(dictMonad);
        }, function () {
            return monadExceptT(dictMonad);
        });
    };
};
module.exports = {
    ExceptT: ExceptT, 
    mapExceptT: mapExceptT, 
    withExceptT: withExceptT, 
    runExceptT: runExceptT, 
    functorExceptT: functorExceptT, 
    applyExceptT: applyExceptT, 
    applicativeExceptT: applicativeExceptT, 
    bindExceptT: bindExceptT, 
    monadExceptT: monadExceptT, 
    monadRecExceptT: monadRecExceptT, 
    altExceptT: altExceptT, 
    plusExceptT: plusExceptT, 
    alternativeExceptT: alternativeExceptT, 
    monadPlusExceptT: monadPlusExceptT, 
    monadTransExceptT: monadTransExceptT, 
    monadEffExceptT: monadEffExceptT, 
    monadContExceptT: monadContExceptT, 
    monadErrorExceptT: monadErrorExceptT, 
    monadReaderExceptT: monadReaderExceptT, 
    monadStateExceptT: monadStateExceptT, 
    monadWriterExceptT: monadWriterExceptT, 
    monadRWSExceptT: monadRWSExceptT
};

},{"../Control.Alt":26,"../Control.Alternative":27,"../Control.Monad.Cont.Class":44,"../Control.Monad.Eff.Class":46,"../Control.Monad.Error.Class":55,"../Control.Monad.RWS.Class":61,"../Control.Monad.Reader.Class":63,"../Control.Monad.Rec.Class":65,"../Control.Monad.State.Class":68,"../Control.Monad.Trans":71,"../Control.Monad.Writer.Class":72,"../Control.MonadPlus":76,"../Control.Plus":77,"../Data.Either":114,"../Data.Monoid":159,"../Data.Tuple":171,"../Prelude":203}],57:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Exists = require("../Data.Exists");
var Data_Either = require("../Data.Either");
var Data_Bifunctor = require("../Data.Bifunctor");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Bound = (function () {
    function Bound(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Bound.create = function (value0) {
        return function (value1) {
            return new Bound(value0, value1);
        };
    };
    return Bound;
})();
var FreeT = (function () {
    function FreeT(value0) {
        this.value0 = value0;
    };
    FreeT.create = function (value0) {
        return new FreeT(value0);
    };
    return FreeT;
})();
var Bind = (function () {
    function Bind(value0) {
        this.value0 = value0;
    };
    Bind.create = function (value0) {
        return new Bind(value0);
    };
    return Bind;
})();
var monadTransFreeT = function (dictFunctor) {
    return new Control_Monad_Trans.MonadTrans(function (dictMonad) {
        return function (ma) {
            return new FreeT(function (v) {
                return Prelude.map(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Left.create)(ma);
            });
        };
    });
};
var freeT = FreeT.create;
var bound = function (m) {
    return function (f) {
        return new Bind(Data_Exists.mkExists(new Bound(m, f)));
    };
};
var functorFreeT = function (dictFunctor) {
    return function (dictFunctor1) {
        return new Prelude.Functor(function (f) {
            return function (v) {
                if (v instanceof FreeT) {
                    return new FreeT(function (v1) {
                        return Prelude.map(dictFunctor1)(Data_Bifunctor.bimap(Data_Either.bifunctorEither)(f)(Prelude.map(dictFunctor)(Prelude.map(functorFreeT(dictFunctor)(dictFunctor1))(f))))(v.value0(Prelude.unit));
                    });
                };
                if (v instanceof Bind) {
                    return Data_Exists.runExists(function (v1) {
                        return bound(v1.value0)(function ($98) {
                            return Prelude.map(functorFreeT(dictFunctor)(dictFunctor1))(f)(v1.value1($98));
                        });
                    })(v.value0);
                };
                throw new Error("Failed pattern match at Control.Monad.Free.Trans line 55, column 3 - line 56, column 3: " + [ f.constructor.name, v.constructor.name ]);
            };
        });
    };
};
var bimapFreeT = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (nf) {
            return function (nm) {
                return function (v) {
                    if (v instanceof Bind) {
                        return Data_Exists.runExists(function (v1) {
                            return bound(function ($99) {
                                return bimapFreeT(dictFunctor)(dictFunctor1)(nf)(nm)(v1.value0($99));
                            })(function ($100) {
                                return bimapFreeT(dictFunctor)(dictFunctor1)(nf)(nm)(v1.value1($100));
                            });
                        })(v.value0);
                    };
                    if (v instanceof FreeT) {
                        return new FreeT(function (v1) {
                            return Prelude["<$>"](dictFunctor1)(Prelude.map(Data_Either.functorEither)(function ($101) {
                                return nf(Prelude.map(dictFunctor)(bimapFreeT(dictFunctor)(dictFunctor1)(nf)(nm))($101));
                            }))(nm(v.value0(Prelude.unit)));
                        });
                    };
                    throw new Error("Failed pattern match at Control.Monad.Free.Trans line 96, column 1 - line 97, column 1: " + [ nf.constructor.name, nm.constructor.name, v.constructor.name ]);
                };
            };
        };
    };
};
var hoistFreeT = function (dictFunctor) {
    return function (dictFunctor1) {
        return bimapFreeT(dictFunctor)(dictFunctor1)(Prelude.id(Prelude.categoryFn));
    };
};
var interpret = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (nf) {
            return bimapFreeT(dictFunctor)(dictFunctor1)(nf)(Prelude.id(Prelude.categoryFn));
        };
    };
};
var monadFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return new Prelude.Monad(function () {
            return applicativeFreeT(dictFunctor)(dictMonad);
        }, function () {
            return bindFreeT(dictFunctor)(dictMonad);
        });
    };
};
var bindFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return new Prelude.Bind(function () {
            return applyFreeT(dictFunctor)(dictMonad);
        }, function (v) {
            return function (f) {
                if (v instanceof Bind) {
                    return Data_Exists.runExists(function (v1) {
                        return bound(v1.value0)(function (x) {
                            return bound(function (v2) {
                                return v1.value1(x);
                            })(f);
                        });
                    })(v.value0);
                };
                return bound(function (v1) {
                    return v;
                })(f);
            };
        });
    };
};
var applyFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return new Prelude.Apply(function () {
            return functorFreeT(dictFunctor)(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
        }, Prelude.ap(monadFreeT(dictFunctor)(dictMonad)));
    };
};
var applicativeFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return new Prelude.Applicative(function () {
            return applyFreeT(dictFunctor)(dictMonad);
        }, function (a) {
            return new FreeT(function (v) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left(a));
            });
        });
    };
};
var liftFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return function (fa) {
            return new FreeT(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(Prelude.map(dictFunctor)(Prelude.pure(applicativeFreeT(dictFunctor)(dictMonad)))(fa)));
            });
        };
    };
};
var resume = function (dictFunctor) {
    return function (dictMonadRec) {
        var go = function (v) {
            if (v instanceof FreeT) {
                return Prelude.map((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Right.create)(v.value0(Prelude.unit));
            };
            if (v instanceof Bind) {
                return Data_Exists.runExists(function (v1) {
                    var $77 = v1.value0(Prelude.unit);
                    if ($77 instanceof FreeT) {
                        return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())($77.value0(Prelude.unit))(function (v2) {
                            if (v2 instanceof Data_Either.Left) {
                                return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left(v1.value1(v2.value0)));
                            };
                            if (v2 instanceof Data_Either.Right) {
                                return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(new Data_Either.Right(Prelude.map(dictFunctor)(function (h) {
                                    return Prelude[">>="](bindFreeT(dictFunctor)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(h)(v1.value1);
                                })(v2.value0))));
                            };
                            throw new Error("Failed pattern match at Control.Monad.Free.Trans line 49, column 9 - line 52, column 7: " + [ v2.constructor.name ]);
                        });
                    };
                    if ($77 instanceof Bind) {
                        return Data_Exists.runExists(function (v2) {
                            return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left(Prelude.bind(bindFreeT(dictFunctor)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(v2.value0(Prelude.unit))(function (z) {
                                return Prelude[">>="](bindFreeT(dictFunctor)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(v2.value1(z))(v1.value1);
                            })));
                        })($77.value0);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Free.Trans line 46, column 5 - line 52, column 100: " + [ $77.constructor.name ]);
                })(v.value0);
            };
            throw new Error("Failed pattern match at Control.Monad.Free.Trans line 44, column 3 - line 45, column 3: " + [ v.constructor.name ]);
        };
        return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
    };
};
var runFreeT = function (dictFunctor) {
    return function (dictMonadRec) {
        return function (interp) {
            var go = function (v) {
                if (v instanceof Data_Either.Left) {
                    return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(v.value0));
                };
                if (v instanceof Data_Either.Right) {
                    return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(interp(v.value0))(function (v1) {
                        return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left(v1));
                    });
                };
                throw new Error("Failed pattern match at Control.Monad.Free.Trans line 104, column 3 - line 105, column 3: " + [ v.constructor.name ]);
            };
            return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(Control_Bind["<=<"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(go)(resume(dictFunctor)(dictMonadRec)));
        };
    };
};
var monadRecFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return new Control_Monad_Rec_Class.MonadRec(function () {
            return monadFreeT(dictFunctor)(dictMonad);
        }, function (f) {
            var go = function (s) {
                return Prelude.bind(bindFreeT(dictFunctor)(dictMonad))(f(s))(function (v) {
                    if (v instanceof Data_Either.Left) {
                        return go(v.value0);
                    };
                    if (v instanceof Data_Either.Right) {
                        return Prelude["return"](applicativeFreeT(dictFunctor)(dictMonad))(v.value0);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Free.Trans line 78, column 7 - line 83, column 1: " + [ v.constructor.name ]);
                });
            };
            return go;
        });
    };
};
module.exports = {
    runFreeT: runFreeT, 
    resume: resume, 
    bimapFreeT: bimapFreeT, 
    interpret: interpret, 
    hoistFreeT: hoistFreeT, 
    liftFreeT: liftFreeT, 
    freeT: freeT, 
    functorFreeT: functorFreeT, 
    applyFreeT: applyFreeT, 
    applicativeFreeT: applicativeFreeT, 
    bindFreeT: bindFreeT, 
    monadFreeT: monadFreeT, 
    monadTransFreeT: monadTransFreeT, 
    monadRecFreeT: monadRecFreeT
};

},{"../Control.Bind":31,"../Control.Monad.Rec.Class":65,"../Control.Monad.Trans":71,"../Data.Bifunctor":104,"../Data.Either":114,"../Data.Exists":117,"../Prelude":203}],58:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Data_CatList = require("../Data.CatList");
var Data_Either = require("../Data.Either");
var Data_Identity = require("../Data.Identity");
var Data_Inject = require("../Data.Inject");
var Data_Maybe = require("../Data.Maybe");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var Data_Tuple = require("../Data.Tuple");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var ExpF = function (x) {
    return x;
};
var Free = (function () {
    function Free(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Free.create = function (value0) {
        return function (value1) {
            return new Free(value0, value1);
        };
    };
    return Free;
})();
var Return = (function () {
    function Return(value0) {
        this.value0 = value0;
    };
    Return.create = function (value0) {
        return new Return(value0);
    };
    return Return;
})();
var Bind = (function () {
    function Bind(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Bind.create = function (value0) {
        return function (value1) {
            return new Bind(value0, value1);
        };
    };
    return Bind;
})();
var toView = function (__copy_v) {
    var v = __copy_v;
    tco: while (true) {
        var runExpF = function (v2) {
            return v2;
        };
        var concatF = function (v2) {
            return function (r) {
                return new Free(v2.value0, Prelude["<>"](Data_CatList.semigroupCatList)(v2.value1)(r));
            };
        };
        if (v.value0 instanceof Return) {
            var $19 = Data_CatList.uncons(v.value1);
            if ($19 instanceof Data_Maybe.Nothing) {
                return new Return(Unsafe_Coerce.unsafeCoerce(v.value0.value0));
            };
            if ($19 instanceof Data_Maybe.Just) {
                var __tco_v = Unsafe_Coerce.unsafeCoerce(concatF(runExpF($19.value0.value0)(v.value0.value0))($19.value0.value1));
                v = __tco_v;
                continue tco;
            };
            throw new Error("Failed pattern match at Control.Monad.Free line 138, column 20 - line 141, column 8: " + [ $19.constructor.name ]);
        };
        if (v.value0 instanceof Bind) {
            return new Bind(v.value0.value0, function (a) {
                return Unsafe_Coerce.unsafeCoerce(concatF(v.value0.value1(a))(v.value1));
            });
        };
        throw new Error("Failed pattern match at Control.Monad.Free line 137, column 3 - line 142, column 3: " + [ v.value0.constructor.name ]);
    };
};
var runFreeM = function (dictFunctor) {
    return function (dictMonadRec) {
        return function (k) {
            var go = function (f) {
                var $28 = toView(f);
                if ($28 instanceof Return) {
                    return Prelude["<$>"]((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Right.create)(Prelude.pure((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())($28.value0));
                };
                if ($28 instanceof Bind) {
                    return Prelude["<$>"]((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Left.create)(k(Prelude["<$>"](dictFunctor)($28.value1)($28.value0)));
                };
                throw new Error("Failed pattern match at Control.Monad.Free line 125, column 5 - line 129, column 1: " + [ $28.constructor.name ]);
            };
            return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
        };
    };
};
var runFree = function (dictFunctor) {
    return function (k) {
        return function ($40) {
            return Data_Identity.runIdentity(runFreeM(dictFunctor)(Control_Monad_Rec_Class.monadRecIdentity)(function ($41) {
                return Data_Identity.Identity(k($41));
            })($40));
        };
    };
};
var fromView = function (f) {
    return new Free(Unsafe_Coerce.unsafeCoerce(f), Data_CatList.empty);
};
var suspendF = function (dictApplicative) {
    return function (f) {
        return fromView(new Bind(Unsafe_Coerce.unsafeCoerce(Prelude.pure(dictApplicative)(f)), function ($42) {
            return Prelude.id(Prelude.categoryFn)(Unsafe_Coerce.unsafeCoerce($42));
        }));
    };
};
var freeMonad = new Prelude.Monad(function () {
    return freeApplicative;
}, function () {
    return freeBind;
});
var freeFunctor = new Prelude.Functor(function (k) {
    return function (f) {
        return Prelude[">>="](freeBind)(f)(function ($43) {
            return Prelude["return"](freeApplicative)(k($43));
        });
    };
});
var freeBind = new Prelude.Bind(function () {
    return freeApply;
}, function (v) {
    return function (k) {
        return new Free(v.value0, Data_CatList.snoc(v.value1)(Unsafe_Coerce.unsafeCoerce(k)));
    };
});
var freeApply = new Prelude.Apply(function () {
    return freeFunctor;
}, Prelude.ap(freeMonad));
var freeApplicative = new Prelude.Applicative(function () {
    return freeApply;
}, function ($44) {
    return fromView(Return.create($44));
});
var freeMonadRec = new Control_Monad_Rec_Class.MonadRec(function () {
    return freeMonad;
}, function (k) {
    return function (a) {
        return Prelude[">>="](freeBind)(k(a))(Data_Either.either(Control_Monad_Rec_Class.tailRecM(freeMonadRec)(k))(Prelude.pure(freeApplicative)));
    };
});
var liftF = function (f) {
    return fromView(new Bind(Unsafe_Coerce.unsafeCoerce(f), function ($45) {
        return Prelude.pure(freeApplicative)(Unsafe_Coerce.unsafeCoerce($45));
    }));
};
var freeMonadTrans = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return liftF;
});
var liftFI = function (dictInject) {
    return function (fa) {
        return liftF(Data_Inject.inj(dictInject)(fa));
    };
};
var foldFree = function (dictMonadRec) {
    return function (k) {
        var go = function (f) {
            var $36 = toView(f);
            if ($36 instanceof Return) {
                return Prelude["<$>"]((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Right.create)(Prelude.pure((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())($36.value0));
            };
            if ($36 instanceof Bind) {
                return Prelude["<$>"]((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function ($46) {
                    return Data_Either.Left.create($36.value1($46));
                })(k($36.value0));
            };
            throw new Error("Failed pattern match at Control.Monad.Free line 110, column 5 - line 115, column 1: " + [ $36.constructor.name ]);
        };
        return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
    };
};
var mapF = function (k) {
    return foldFree(freeMonadRec)(function ($47) {
        return liftF(k($47));
    });
};
var injF = function (dictInject) {
    return mapF(Data_Inject.inj(dictInject));
};
module.exports = {
    runFreeM: runFreeM, 
    runFree: runFree, 
    foldFree: foldFree, 
    injF: injF, 
    mapF: mapF, 
    liftFI: liftFI, 
    liftF: liftF, 
    suspendF: suspendF, 
    freeFunctor: freeFunctor, 
    freeBind: freeBind, 
    freeApplicative: freeApplicative, 
    freeApply: freeApply, 
    freeMonad: freeMonad, 
    freeMonadTrans: freeMonadTrans, 
    freeMonadRec: freeMonadRec
};

},{"../Control.Monad.Rec.Class":65,"../Control.Monad.Trans":71,"../Data.CatList":106,"../Data.Either":114,"../Data.Identity":137,"../Data.Inject":138,"../Data.Maybe":152,"../Data.NaturalTransformation":160,"../Data.Tuple":171,"../Prelude":203,"../Unsafe.Coerce":206}],59:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_Plus = require("../Control.Plus");
var Data_Lazy = require("../Data.Lazy");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Yield = (function () {
    function Yield(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Yield.create = function (value0) {
        return function (value1) {
            return new Yield(value0, value1);
        };
    };
    return Yield;
})();
var Skip = (function () {
    function Skip(value0) {
        this.value0 = value0;
    };
    Skip.create = function (value0) {
        return new Skip(value0);
    };
    return Skip;
})();
var Done = (function () {
    function Done() {

    };
    Done.value = new Done();
    return Done;
})();
var ListT = (function () {
    function ListT(value0) {
        this.value0 = value0;
    };
    ListT.create = function (value0) {
        return new ListT(value0);
    };
    return ListT;
})();
var wrapLazy = function (dictApplicative) {
    return function (v) {
        return ListT.create(Prelude.pure(dictApplicative)(new Skip(v)));
    };
};
var wrapEffect = function (dictFunctor) {
    return function (v) {
        return ListT.create(Prelude["<$>"](dictFunctor)(function ($159) {
            return Skip.create(Data_Lazy.defer(Prelude["const"]($159)));
        })(v));
    };
};
var unfold = function (dictMonad) {
    return function (f) {
        return function (z) {
            var g = function (v) {
                if (v instanceof Data_Maybe.Just) {
                    return new Yield(v.value0.value1, Data_Lazy.defer(function (v1) {
                        return unfold(dictMonad)(f)(v.value0.value0);
                    }));
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 118, column 3 - line 119, column 3: " + [ v.constructor.name ]);
            };
            return ListT.create(Prelude["<$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(g)(f(z)));
        };
    };
};
var runListT = function (v) {
    return v.value0;
};
var scanl = function (dictMonad) {
    return function (f) {
        return function (b) {
            return function (l) {
                var g = function (v) {
                    var h = function (v1) {
                        if (v1 instanceof Yield) {
                            var b$prime = f(v.value0)(v1.value0);
                            return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(b$prime, Data_Lazy.force(v1.value1)), b$prime));
                        };
                        if (v1 instanceof Skip) {
                            return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0, Data_Lazy.force(v1.value0)), v.value0));
                        };
                        if (v1 instanceof Done) {
                            return Data_Maybe.Nothing.value;
                        };
                        throw new Error("Failed pattern match at Control.Monad.List.Trans line 217, column 5 - line 219, column 5: " + [ v1.constructor.name ]);
                    };
                    return Prelude["<$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(h)(runListT(v.value1));
                };
                return unfold(dictMonad)(g)(new Data_Tuple.Tuple(b, l));
            };
        };
    };
};
var stepMap = function (dictFunctor) {
    return function (f) {
        return function (l) {
            return ListT.create(Prelude["<$>"](dictFunctor)(f)(runListT(l)));
        };
    };
};
var takeWhile = function (dictApplicative) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var $88 = f(v.value0);
                if ($88) {
                    return new Yield(v.value0, Prelude["<$>"](Data_Lazy.functorLazy)(takeWhile(dictApplicative)(f))(v.value1));
                };
                if (!$88) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 142, column 19 - line 143, column 3: " + [ $88.constructor.name ]);
            };
            if (v instanceof Skip) {
                return Skip.create(Prelude["<$>"](Data_Lazy.functorLazy)(takeWhile(dictApplicative)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 142, column 3 - line 143, column 3: " + [ v.constructor.name ]);
        };
        return stepMap((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(g);
    };
};
var uncons = function (dictMonad) {
    return function (l) {
        var g = function (v) {
            if (v instanceof Yield) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Maybe.Just.create(new Data_Tuple.Tuple(v.value0, Data_Lazy.force(v.value1))));
            };
            if (v instanceof Skip) {
                return uncons(dictMonad)(Data_Lazy.force(v.value0));
            };
            if (v instanceof Done) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Maybe.Nothing.value);
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 183, column 3 - line 184, column 3: " + [ v.constructor.name ]);
        };
        return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runListT(l))(g);
    };
};
var tail = function (dictMonad) {
    return function (l) {
        return Prelude["<$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Tuple.snd))(uncons(dictMonad)(l));
    };
};
var prepend$prime = function (dictApplicative) {
    return function (h) {
        return function (t) {
            return ListT.create(Prelude.pure(dictApplicative)(new Yield(h, t)));
        };
    };
};
var prepend = function (dictApplicative) {
    return function (h) {
        return function (t) {
            return prepend$prime(dictApplicative)(h)(Data_Lazy.defer(Prelude["const"](t)));
        };
    };
};
var nil = function (dictApplicative) {
    return ListT.create(Prelude.pure(dictApplicative)(Done.value));
};
var singleton = function (dictApplicative) {
    return function (a) {
        return prepend(dictApplicative)(a)(nil(dictApplicative));
    };
};
var take = function (dictApplicative) {
    return function (v) {
        return function (fa) {
            if (v === 0) {
                return nil(dictApplicative);
            };
            var f = function (v1) {
                if (v1 instanceof Yield) {
                    return new Yield(v1.value0, Prelude["<$>"](Data_Lazy.functorLazy)(take(dictApplicative)(v - 1))(v1.value1));
                };
                if (v1 instanceof Skip) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(take(dictApplicative)(v))(v1.value0));
                };
                if (v1 instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 135, column 3 - line 136, column 3: " + [ v1.constructor.name ]);
            };
            return stepMap((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(f)(fa);
        };
    };
};
var zipWith$prime = function (dictMonad) {
    return function (f) {
        var g = function (v) {
            return function (v1) {
                if (v1 instanceof Data_Maybe.Nothing) {
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(nil(dictMonad["__superclass_Prelude.Applicative_0"]()));
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(nil(dictMonad["__superclass_Prelude.Applicative_0"]()));
                };
                if (v instanceof Data_Maybe.Just && v1 instanceof Data_Maybe.Just) {
                    return Prelude["<$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude.flip(prepend$prime(dictMonad["__superclass_Prelude.Applicative_0"]()))(Data_Lazy.defer(function (v2) {
                        return zipWith$prime(dictMonad)(f)(v.value0.value1)(v1.value0.value1);
                    })))(f(v.value0.value0)(v1.value0.value0));
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 225, column 3 - line 230, column 3: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        var loop = function (fa) {
            return function (fb) {
                return wrapEffect(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(uncons(dictMonad)(fa))(function (v) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(uncons(dictMonad)(fb))(function (v1) {
                        return g(v)(v1);
                    });
                }));
            };
        };
        return loop;
    };
};
var zipWith = function (dictMonad) {
    return function (f) {
        var g = function (a) {
            return function (b) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(f(a)(b));
            };
        };
        return zipWith$prime(dictMonad)(g);
    };
};
var mapMaybe = function (dictFunctor) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                return Data_Maybe.fromMaybe(Skip.create)(Prelude["<$>"](Data_Maybe.functorMaybe)(Yield.create)(f(v.value0)))(Prelude["<$>"](Data_Lazy.functorLazy)(mapMaybe(dictFunctor)(f))(v.value1));
            };
            if (v instanceof Skip) {
                return Skip.create(Prelude["<$>"](Data_Lazy.functorLazy)(mapMaybe(dictFunctor)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 171, column 3 - line 172, column 3: " + [ v.constructor.name ]);
        };
        return stepMap(dictFunctor)(g);
    };
};
var iterate = function (dictMonad) {
    return function (f) {
        return function (a) {
            var g = function (a1) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Maybe.Just(new Data_Tuple.Tuple(f(a1), a1)));
            };
            return unfold(dictMonad)(g)(a);
        };
    };
};
var repeat = function (dictMonad) {
    return iterate(dictMonad)(Prelude.id(Prelude.categoryFn));
};
var head = function (dictMonad) {
    return function (l) {
        return Prelude["<$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Tuple.fst))(uncons(dictMonad)(l));
    };
};
var functorListT = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                return new Yield(f(v.value0), Prelude["<$>"](Data_Lazy.functorLazy)(Prelude["<$>"](functorListT(dictFunctor))(f))(v.value1));
            };
            if (v instanceof Skip) {
                return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(Prelude["<$>"](functorListT(dictFunctor))(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 248, column 5 - line 249, column 5: " + [ v.constructor.name ]);
        };
        return stepMap(dictFunctor)(g);
    });
};
var fromEffect = function (dictApplicative) {
    return function (fa) {
        return ListT.create(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function (v) {
            return new Yield(v, Data_Lazy.defer(function (v1) {
                return nil(dictApplicative);
            }));
        })(fa));
    };
};
var monadTransListT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return fromEffect(dictMonad["__superclass_Prelude.Applicative_0"]());
});
var foldl$prime = function (dictMonad) {
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(b);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(f(b)(v.value0.value0))(Prelude.flip(loop)(v.value0.value1));
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans line 200, column 5 - line 201, column 5: " + [ v.constructor.name ]);
                };
                return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(uncons(dictMonad)(l))(g);
            };
        };
        return loop;
    };
};
var foldl = function (dictMonad) {
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(b);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return loop(f(b)(v.value0.value0))(v.value0.value1);
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans line 208, column 5 - line 209, column 5: " + [ v.constructor.name ]);
                };
                return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(uncons(dictMonad)(l))(g);
            };
        };
        return loop;
    };
};
var filter = function (dictFunctor) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var s$prime = Prelude["<$>"](Data_Lazy.functorLazy)(filter(dictFunctor)(f))(v.value1);
                var $131 = f(v.value0);
                if ($131) {
                    return new Yield(v.value0, s$prime);
                };
                if (!$131) {
                    return new Skip(s$prime);
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 164, column 19 - line 164, column 55: " + [ $131.constructor.name ]);
            };
            if (v instanceof Skip) {
                var s$prime = Prelude["<$>"](Data_Lazy.functorLazy)(filter(dictFunctor)(f))(v.value0);
                return new Skip(s$prime);
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 164, column 3 - line 165, column 3: " + [ v.constructor.name ]);
        };
        return stepMap(dictFunctor)(g);
    };
};
var dropWhile = function (dictApplicative) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var $136 = f(v.value0);
                if ($136) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(dropWhile(dictApplicative)(f))(v.value1));
                };
                if (!$136) {
                    return new Yield(v.value0, v.value1);
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 157, column 19 - line 158, column 3: " + [ $136.constructor.name ]);
            };
            if (v instanceof Skip) {
                return Skip.create(Prelude["<$>"](Data_Lazy.functorLazy)(dropWhile(dictApplicative)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 157, column 3 - line 158, column 3: " + [ v.constructor.name ]);
        };
        return stepMap((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(g);
    };
};
var drop = function (dictApplicative) {
    return function (v) {
        return function (fa) {
            if (v === 0) {
                return fa;
            };
            var f = function (v1) {
                if (v1 instanceof Yield) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(drop(dictApplicative)(v - 1))(v1.value1));
                };
                if (v1 instanceof Skip) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(drop(dictApplicative)(v))(v1.value0));
                };
                if (v1 instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 150, column 3 - line 151, column 3: " + [ v1.constructor.name ]);
            };
            return stepMap((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(f)(fa);
        };
    };
};
var cons = function (dictApplicative) {
    return function (lh) {
        return function (t) {
            return ListT.create(Prelude.pure(dictApplicative)(new Yield(Data_Lazy.force(lh), t)));
        };
    };
};
var unfoldableListT = function (dictMonad) {
    return new Data_Unfoldable.Unfoldable(function (f) {
        return function (b) {
            var go = function (v) {
                if (v instanceof Data_Maybe.Nothing) {
                    return nil(dictMonad["__superclass_Prelude.Applicative_0"]());
                };
                if (v instanceof Data_Maybe.Just) {
                    return cons(dictMonad["__superclass_Prelude.Applicative_0"]())(Prelude.pure(Data_Lazy.applicativeLazy)(v.value0.value0))(Data_Lazy.defer(function (v1) {
                        return go(f(v.value0.value1));
                    }));
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 253, column 3 - line 257, column 1: " + [ v.constructor.name ]);
            };
            return go(f(b));
        };
    });
};
var semigroupListT = function (dictApplicative) {
    return new Prelude.Semigroup(concat(dictApplicative));
};
var concat = function (dictApplicative) {
    return function (x) {
        return function (y) {
            var f = function (v) {
                if (v instanceof Yield) {
                    return new Yield(v.value0, Prelude["<$>"](Data_Lazy.functorLazy)(function (v1) {
                        return Prelude["<>"](semigroupListT(dictApplicative))(v1)(y);
                    })(v.value1));
                };
                if (v instanceof Skip) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(function (v1) {
                        return Prelude["<>"](semigroupListT(dictApplicative))(v1)(y);
                    })(v.value0));
                };
                if (v instanceof Done) {
                    return new Skip(Data_Lazy.defer(Prelude["const"](y)));
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 94, column 3 - line 95, column 3: " + [ v.constructor.name ]);
            };
            return stepMap((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(f)(x);
        };
    };
};
var monoidListT = function (dictApplicative) {
    return new Data_Monoid.Monoid(function () {
        return semigroupListT(dictApplicative);
    }, nil(dictApplicative));
};
var catMaybes = function (dictFunctor) {
    return mapMaybe(dictFunctor)(Prelude.id(Prelude.categoryFn));
};
var monadListT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeListT(dictMonad);
    }, function () {
        return bindListT(dictMonad);
    });
};
var bindListT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyListT(dictMonad);
    }, function (fa) {
        return function (f) {
            var g = function (v) {
                if (v instanceof Yield) {
                    var h = function (s1) {
                        return Prelude["<>"](semigroupListT(dictMonad["__superclass_Prelude.Applicative_0"]()))(f(v.value0))(Prelude[">>="](bindListT(dictMonad))(s1)(f));
                    };
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(h)(v.value1));
                };
                if (v instanceof Skip) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(function (v1) {
                        return Prelude[">>="](bindListT(dictMonad))(v1)(f);
                    })(v.value0));
                };
                if (v instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 265, column 5 - line 268, column 5: " + [ v.constructor.name ]);
            };
            return stepMap(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(g)(fa);
        };
    });
};
var applyListT = function (dictMonad) {
    return new Prelude.Apply(function () {
        return functorListT(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
    }, Prelude.ap(monadListT(dictMonad)));
};
var applicativeListT = function (dictMonad) {
    return new Prelude.Applicative(function () {
        return applyListT(dictMonad);
    }, singleton(dictMonad["__superclass_Prelude.Applicative_0"]()));
};
var monadEffListT = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadListT(dictMonadEff["__superclass_Prelude.Monad_0"]());
    }, function ($160) {
        return Control_Monad_Trans.lift(monadTransListT)(dictMonadEff["__superclass_Prelude.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($160));
    });
};
var altListT = function (dictApplicative) {
    return new Control_Alt.Alt(function () {
        return functorListT((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
    }, concat(dictApplicative));
};
var plusListT = function (dictMonad) {
    return new Control_Plus.Plus(function () {
        return altListT(dictMonad["__superclass_Prelude.Applicative_0"]());
    }, nil(dictMonad["__superclass_Prelude.Applicative_0"]()));
};
var alternativeListT = function (dictMonad) {
    return new Control_Alternative.Alternative(function () {
        return plusListT(dictMonad);
    }, function () {
        return applicativeListT(dictMonad);
    });
};
var monadPlusListT = function (dictMonad) {
    return new Control_MonadPlus.MonadPlus(function () {
        return alternativeListT(dictMonad);
    }, function () {
        return monadListT(dictMonad);
    });
};
module.exports = {
    "zipWith'": zipWith$prime, 
    zipWith: zipWith, 
    wrapLazy: wrapLazy, 
    wrapEffect: wrapEffect, 
    unfold: unfold, 
    uncons: uncons, 
    takeWhile: takeWhile, 
    take: take, 
    tail: tail, 
    singleton: singleton, 
    scanl: scanl, 
    repeat: repeat, 
    "prepend'": prepend$prime, 
    prepend: prepend, 
    nil: nil, 
    mapMaybe: mapMaybe, 
    iterate: iterate, 
    head: head, 
    fromEffect: fromEffect, 
    "foldl'": foldl$prime, 
    foldl: foldl, 
    filter: filter, 
    dropWhile: dropWhile, 
    drop: drop, 
    cons: cons, 
    catMaybes: catMaybes, 
    semigroupListT: semigroupListT, 
    monoidListT: monoidListT, 
    functorListT: functorListT, 
    unfoldableListT: unfoldableListT, 
    applyListT: applyListT, 
    applicativeListT: applicativeListT, 
    bindListT: bindListT, 
    monadListT: monadListT, 
    monadTransListT: monadTransListT, 
    altListT: altListT, 
    plusListT: plusListT, 
    alternativeListT: alternativeListT, 
    monadPlusListT: monadPlusListT, 
    monadEffListT: monadEffListT
};

},{"../Control.Alt":26,"../Control.Alternative":27,"../Control.Monad.Eff.Class":46,"../Control.Monad.Trans":71,"../Control.MonadPlus":76,"../Control.Plus":77,"../Data.Lazy":145,"../Data.Maybe":152,"../Data.Monoid":159,"../Data.Tuple":171,"../Data.Unfoldable":172,"../Prelude":203}],60:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Maybe = require("../Data.Maybe");
var Data_Tuple = require("../Data.Tuple");
var Data_Monoid = require("../Data.Monoid");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_RWS_Class = require("../Control.Monad.RWS.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_Plus = require("../Control.Plus");
var MaybeT = function (x) {
    return x;
};
var runMaybeT = function (v) {
    return v;
};
var monadTransMaybeT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function ($46) {
        return MaybeT(Prelude.liftM1(dictMonad)(Data_Maybe.Just.create)($46));
    };
});
var mapMaybeT = function (f) {
    return function ($47) {
        return MaybeT(f(runMaybeT($47)));
    };
};
var monadMaybeT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeMaybeT(dictMonad);
    }, function () {
        return bindMaybeT(dictMonad);
    });
};
var functorMaybeT = function (dictMonad) {
    return new Prelude.Functor(Prelude.liftA1(applicativeMaybeT(dictMonad)));
};
var bindMaybeT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyMaybeT(dictMonad);
    }, function (x) {
        return function (f) {
            return MaybeT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runMaybeT(x))(function (v) {
                if (v instanceof Data_Maybe.Nothing) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Maybe.Nothing.value);
                };
                if (v instanceof Data_Maybe.Just) {
                    return runMaybeT(f(v.value0));
                };
                throw new Error("Failed pattern match at Control.Monad.Maybe.Trans line 55, column 5 - line 59, column 1: " + [ v.constructor.name ]);
            }));
        };
    });
};
var applyMaybeT = function (dictMonad) {
    return new Prelude.Apply(function () {
        return functorMaybeT(dictMonad);
    }, Prelude.ap(monadMaybeT(dictMonad)));
};
var applicativeMaybeT = function (dictMonad) {
    return new Prelude.Applicative(function () {
        return applyMaybeT(dictMonad);
    }, function ($48) {
        return MaybeT(Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Maybe.Just.create($48)));
    });
};
var monadContMaybeT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadMaybeT(dictMonadCont["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return MaybeT(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
            return runMaybeT(f(function (a) {
                return MaybeT(c(new Data_Maybe.Just(a)));
            }));
        }));
    });
};
var monadEffMaybe = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadMaybeT(dictMonadEff["__superclass_Prelude.Monad_0"]());
    }, function ($49) {
        return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadEff["__superclass_Prelude.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($49));
    });
};
var monadErrorMaybeT = function (dictMonadError) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadMaybeT(dictMonadError["__superclass_Prelude.Monad_0"]());
    }, function (m) {
        return function (h) {
            return MaybeT(Control_Monad_Error_Class.catchError(dictMonadError)(runMaybeT(m))(function ($50) {
                return runMaybeT(h($50));
            }));
        };
    }, function (e) {
        return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadError["__superclass_Prelude.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
    });
};
var monadReaderMaybeT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadMaybeT(dictMonadReader["__superclass_Prelude.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadReader["__superclass_Prelude.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return mapMaybeT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
    });
};
var monadRecMaybeT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadMaybeT(dictMonadRec["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return function ($51) {
            return MaybeT(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(function (a) {
                return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runMaybeT(f(a)))(function (v) {
                    return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                        if (v instanceof Data_Maybe.Nothing) {
                            return new Data_Either.Right(Data_Maybe.Nothing.value);
                        };
                        if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Left) {
                            return new Data_Either.Left(v.value0.value0);
                        };
                        if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Right) {
                            return new Data_Either.Right(new Data_Maybe.Just(v.value0.value0));
                        };
                        throw new Error("Failed pattern match at Control.Monad.Maybe.Trans line 81, column 5 - line 86, column 1: " + [ v.constructor.name ]);
                    })());
                });
            })($51));
        };
    });
};
var monadStateMaybeT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadMaybeT(dictMonadState["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadState["__superclass_Prelude.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
};
var monadWriterMaybeT = function (dictMonad) {
    return function (dictMonadWriter) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadMaybeT(dictMonad);
        }, mapMaybeT(function (m) {
            return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m))(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Prelude["<$>"](Data_Maybe.functorMaybe)(function (r) {
                    return new Data_Tuple.Tuple(r, v.value1);
                })(v.value0));
            });
        }), mapMaybeT(function (m) {
            return Control_Monad_Writer_Class.pass(dictMonadWriter)(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())((function () {
                    if (v instanceof Data_Maybe.Nothing) {
                        return new Data_Tuple.Tuple(Data_Maybe.Nothing.value, Prelude.id(Prelude.categoryFn));
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return new Data_Tuple.Tuple(new Data_Maybe.Just(v.value0.value0), v.value0.value1);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Maybe.Trans line 110, column 5 - line 114, column 1: " + [ v.constructor.name ]);
                })());
            }));
        }), function (wd) {
            return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonad)(Control_Monad_Writer_Class.writer(dictMonadWriter)(wd));
        });
    };
};
var monadRWSMaybeT = function (dictMonoid) {
    return function (dictMonadRWS) {
        return new Control_Monad_RWS_Class.MonadRWS(function () {
            return monadReaderMaybeT(dictMonadRWS["__superclass_Control.Monad.Reader.Class.MonadReader_1"]());
        }, function () {
            return monadStateMaybeT(dictMonadRWS["__superclass_Control.Monad.State.Class.MonadState_3"]());
        }, function () {
            return monadWriterMaybeT((dictMonadRWS["__superclass_Control.Monad.State.Class.MonadState_3"]())["__superclass_Prelude.Monad_0"]())(dictMonadRWS["__superclass_Control.Monad.Writer.Class.MonadWriter_2"]());
        }, function () {
            return dictMonoid;
        });
    };
};
var altMaybeT = function (dictMonad) {
    return new Control_Alt.Alt(function () {
        return functorMaybeT(dictMonad);
    }, function (m1) {
        return function (m2) {
            return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runMaybeT(m1))(function (v) {
                if (v instanceof Data_Maybe.Nothing) {
                    return runMaybeT(m2);
                };
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(v);
            });
        };
    });
};
var plusMaybeT = function (dictMonad) {
    return new Control_Plus.Plus(function () {
        return altMaybeT(dictMonad);
    }, Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Maybe.Nothing.value));
};
var alternativeMaybeT = function (dictMonad) {
    return new Control_Alternative.Alternative(function () {
        return plusMaybeT(dictMonad);
    }, function () {
        return applicativeMaybeT(dictMonad);
    });
};
var monadPlusMaybeT = function (dictMonad) {
    return new Control_MonadPlus.MonadPlus(function () {
        return alternativeMaybeT(dictMonad);
    }, function () {
        return monadMaybeT(dictMonad);
    });
};
module.exports = {
    MaybeT: MaybeT, 
    mapMaybeT: mapMaybeT, 
    runMaybeT: runMaybeT, 
    functorMaybeT: functorMaybeT, 
    applyMaybeT: applyMaybeT, 
    applicativeMaybeT: applicativeMaybeT, 
    bindMaybeT: bindMaybeT, 
    monadMaybeT: monadMaybeT, 
    monadTransMaybeT: monadTransMaybeT, 
    altMaybeT: altMaybeT, 
    plusMaybeT: plusMaybeT, 
    alternativeMaybeT: alternativeMaybeT, 
    monadPlusMaybeT: monadPlusMaybeT, 
    monadRecMaybeT: monadRecMaybeT, 
    monadEffMaybe: monadEffMaybe, 
    monadContMaybeT: monadContMaybeT, 
    monadErrorMaybeT: monadErrorMaybeT, 
    monadReaderMaybeT: monadReaderMaybeT, 
    monadStateMaybeT: monadStateMaybeT, 
    monadWriterMaybeT: monadWriterMaybeT, 
    monadRWSMaybeT: monadRWSMaybeT
};

},{"../Control.Alt":26,"../Control.Alternative":27,"../Control.Monad.Cont.Class":44,"../Control.Monad.Eff.Class":46,"../Control.Monad.Error.Class":55,"../Control.Monad.RWS.Class":61,"../Control.Monad.Reader.Class":63,"../Control.Monad.Rec.Class":65,"../Control.Monad.State.Class":68,"../Control.Monad.Trans":71,"../Control.Monad.Writer.Class":72,"../Control.MonadPlus":76,"../Control.Plus":77,"../Data.Either":114,"../Data.Maybe":152,"../Data.Monoid":159,"../Data.Tuple":171,"../Prelude":203}],61:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Monoid = require("../Data.Monoid");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var MonadRWS = function (__superclass_Control$dotMonad$dotReader$dotClass$dotMonadReader_1, __superclass_Control$dotMonad$dotState$dotClass$dotMonadState_3, __superclass_Control$dotMonad$dotWriter$dotClass$dotMonadWriter_2, __superclass_Data$dotMonoid$dotMonoid_0) {
    this["__superclass_Control.Monad.Reader.Class.MonadReader_1"] = __superclass_Control$dotMonad$dotReader$dotClass$dotMonadReader_1;
    this["__superclass_Control.Monad.State.Class.MonadState_3"] = __superclass_Control$dotMonad$dotState$dotClass$dotMonadState_3;
    this["__superclass_Control.Monad.Writer.Class.MonadWriter_2"] = __superclass_Control$dotMonad$dotWriter$dotClass$dotMonadWriter_2;
    this["__superclass_Data.Monoid.Monoid_0"] = __superclass_Data$dotMonoid$dotMonoid_0;
};
module.exports = {
    MonadRWS: MonadRWS
};

},{"../Control.Monad.Reader.Class":63,"../Control.Monad.State.Class":68,"../Control.Monad.Trans":71,"../Control.Monad.Writer.Class":72,"../Data.Monoid":159,"../Prelude":203}],62:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Monoid = require("../Data.Monoid");
var Data_Tuple = require("../Data.Tuple");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_RWS_Class = require("../Control.Monad.RWS.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var RWSResult = (function () {
    function RWSResult(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    RWSResult.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new RWSResult(value0, value1, value2);
            };
        };
    };
    return RWSResult;
})();
var RWST = function (x) {
    return x;
};
var runRWST = function (v) {
    return v;
};
var withRWST = function (f) {
    return function (m) {
        return function (r) {
            return function (s) {
                return Data_Tuple.uncurry(runRWST(m))(f(r)(s));
            };
        };
    };
};
var monadTransRWST = function (dictMonoid) {
    return new Control_Monad_Trans.MonadTrans(function (dictMonad) {
        return function (m) {
            return function (v) {
                return function (s) {
                    return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (a) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(s, a, Data_Monoid.mempty(dictMonoid)));
                    });
                };
            };
        };
    });
};
var mapRWST = function (f) {
    return function (m) {
        return function (r) {
            return function (s) {
                return f(runRWST(m)(r)(s));
            };
        };
    };
};
var functorRWST = function (dictFunctor) {
    return function (dictMonoid) {
        return new Prelude.Functor(function (f) {
            return function (m) {
                return function (r) {
                    return function (s) {
                        return Prelude["<$>"](dictFunctor)(function (v) {
                            return new RWSResult(v.value0, f(v.value1), v.value2);
                        })(runRWST(m)(r)(s));
                    };
                };
            };
        });
    };
};
var execRWST = function (dictMonad) {
    return function (m) {
        return function (r) {
            return function (s) {
                return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runRWST(m)(r)(s))(function (v) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v.value0, v.value2));
                });
            };
        };
    };
};
var evalRWST = function (dictMonad) {
    return function (m) {
        return function (r) {
            return function (s) {
                return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runRWST(m)(r)(s))(function (v) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v.value1, v.value2));
                });
            };
        };
    };
};
var applyRWST = function (dictBind) {
    return function (dictMonoid) {
        return new Prelude.Apply(function () {
            return functorRWST((dictBind["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(dictMonoid);
        }, function (f) {
            return function (m) {
                return function (r) {
                    return function (s) {
                        return Prelude[">>="](dictBind)(runRWST(f)(r)(s))(function (v) {
                            return Prelude["<#>"]((dictBind["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(runRWST(m)(r)(v.value0))(function (v1) {
                                return new RWSResult(v1.value0, v.value1(v1.value1), Prelude["++"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value2)(v1.value2));
                            });
                        });
                    };
                };
            };
        });
    };
};
var bindRWST = function (dictBind) {
    return function (dictMonoid) {
        return new Prelude.Bind(function () {
            return applyRWST(dictBind)(dictMonoid);
        }, function (m) {
            return function (f) {
                return function (r) {
                    return function (s) {
                        return Prelude[">>="](dictBind)(runRWST(m)(r)(s))(function (v) {
                            return Prelude["<#>"]((dictBind["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(runRWST(f(v.value1))(r)(v.value0))(function (v1) {
                                return new RWSResult(v1.value0, v1.value1, Prelude["++"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value2)(v1.value2));
                            });
                        });
                    };
                };
            };
        });
    };
};
var applicativeRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Prelude.Applicative(function () {
            return applyRWST(dictMonad["__superclass_Prelude.Bind_1"]())(dictMonoid);
        }, function (a) {
            return function (v) {
                return function (s) {
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(s, a, Data_Monoid.mempty(dictMonoid)));
                };
            };
        });
    };
};
var monadRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Prelude.Monad(function () {
            return applicativeRWST(dictMonad)(dictMonoid);
        }, function () {
            return bindRWST(dictMonad["__superclass_Prelude.Bind_1"]())(dictMonoid);
        });
    };
};
var monadEffRWS = function (dictMonad) {
    return function (dictMonoid) {
        return function (dictMonadEff) {
            return new Control_Monad_Eff_Class.MonadEff(function () {
                return monadRWST(dictMonad)(dictMonoid);
            }, function ($107) {
                return Control_Monad_Trans.lift(monadTransRWST(dictMonoid))(dictMonad)(Control_Monad_Eff_Class.liftEff(dictMonadEff)($107));
            });
        };
    };
};
var monadErrorRWST = function (dictMonadError) {
    return function (dictMonoid) {
        return new Control_Monad_Error_Class.MonadError(function () {
            return monadRWST(dictMonadError["__superclass_Prelude.Monad_0"]())(dictMonoid);
        }, function (m) {
            return function (h) {
                return RWST(function (r) {
                    return function (s) {
                        return Control_Monad_Error_Class.catchError(dictMonadError)(runRWST(m)(r)(s))(function (e) {
                            return runRWST(h(e))(r)(s);
                        });
                    };
                });
            };
        }, function (e) {
            return Control_Monad_Trans.lift(monadTransRWST(dictMonoid))(dictMonadError["__superclass_Prelude.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
        });
    };
};
var monadReaderRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Control_Monad_Reader_Class.MonadReader(function () {
            return monadRWST(dictMonad)(dictMonoid);
        }, function (r) {
            return function (s) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(s, r, Data_Monoid.mempty(dictMonoid)));
            };
        }, function (f) {
            return function (m) {
                return function (r) {
                    return function (s) {
                        return runRWST(m)(f(r))(s);
                    };
                };
            };
        });
    };
};
var monadRecRWST = function (dictMonoid) {
    return function (dictMonadRec) {
        return new Control_Monad_Rec_Class.MonadRec(function () {
            return monadRWST(dictMonadRec["__superclass_Prelude.Monad_0"]())(dictMonoid);
        }, function (k) {
            return function (a) {
                var k$prime = function (r) {
                    return function (v) {
                        return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runRWST(k(v.value1))(r)(v.value0))(function (v1) {
                            return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                                if (v1.value1 instanceof Data_Either.Left) {
                                    return new Data_Either.Left(new RWSResult(v1.value0, v1.value1.value0, Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value2)(v1.value2)));
                                };
                                if (v1.value1 instanceof Data_Either.Right) {
                                    return new Data_Either.Right(new RWSResult(v1.value0, v1.value1.value0, Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value2)(v1.value2)));
                                };
                                throw new Error("Failed pattern match at Control.Monad.RWS.Trans line 100, column 7 - line 102, column 75: " + [ v1.value1.constructor.name ]);
                            })());
                        });
                    };
                };
                return function (r) {
                    return function (s) {
                        return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(k$prime(r))(new RWSResult(s, a, Data_Monoid.mempty(dictMonoid)));
                    };
                };
            };
        });
    };
};
var monadStateRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Control_Monad_State_Class.MonadState(function () {
            return monadRWST(dictMonad)(dictMonoid);
        }, function (f) {
            return function (v) {
                return function (s) {
                    var $90 = f(s);
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult($90.value1, $90.value0, Data_Monoid.mempty(dictMonoid)));
                };
            };
        });
    };
};
var monadWriterRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadRWST(dictMonad)(dictMonoid);
        }, function (m) {
            return function (r) {
                return function (s) {
                    return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runRWST(m)(r)(s))(function (v) {
                        return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(v.value0, new Data_Tuple.Tuple(v.value1, v.value2), v.value2));
                    });
                };
            };
        }, function (m) {
            return function (r) {
                return function (s) {
                    return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runRWST(m)(r)(s))(function (v) {
                        return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(v.value0, v.value1.value0, v.value1.value1(v.value2)));
                    });
                };
            };
        }, function (v) {
            return function (v1) {
                return function (s) {
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(s, v.value0, v.value1));
                };
            };
        });
    };
};
var monadRWSRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Control_Monad_RWS_Class.MonadRWS(function () {
            return monadReaderRWST(dictMonad)(dictMonoid);
        }, function () {
            return monadStateRWST(dictMonad)(dictMonoid);
        }, function () {
            return monadWriterRWST(dictMonad)(dictMonoid);
        }, function () {
            return dictMonoid;
        });
    };
};
module.exports = {
    RWST: RWST, 
    RWSResult: RWSResult, 
    withRWST: withRWST, 
    mapRWST: mapRWST, 
    execRWST: execRWST, 
    evalRWST: evalRWST, 
    runRWST: runRWST, 
    functorRWST: functorRWST, 
    applyRWST: applyRWST, 
    bindRWST: bindRWST, 
    applicativeRWST: applicativeRWST, 
    monadRWST: monadRWST, 
    monadTransRWST: monadTransRWST, 
    monadEffRWS: monadEffRWS, 
    monadReaderRWST: monadReaderRWST, 
    monadStateRWST: monadStateRWST, 
    monadWriterRWST: monadWriterRWST, 
    monadRWSRWST: monadRWSRWST, 
    monadErrorRWST: monadErrorRWST, 
    monadRecRWST: monadRecRWST
};

},{"../Control.Monad.Eff.Class":46,"../Control.Monad.Error.Class":55,"../Control.Monad.RWS.Class":61,"../Control.Monad.Reader.Class":63,"../Control.Monad.Rec.Class":65,"../Control.Monad.State.Class":68,"../Control.Monad.Trans":71,"../Control.Monad.Writer.Class":72,"../Data.Either":114,"../Data.Monoid":159,"../Data.Tuple":171,"../Prelude":203}],63:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var MonadReader = function (__superclass_Prelude$dotMonad_0, ask, local) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.ask = ask;
    this.local = local;
};
var monadReaderFun = new MonadReader(function () {
    return Prelude.monadFn;
}, Prelude.id(Prelude.categoryFn), Prelude[">>>"](Prelude.semigroupoidFn));
var local = function (dict) {
    return dict.local;
};
var ask = function (dict) {
    return dict.ask;
};
var reader = function (dictMonadReader) {
    return function (f) {
        return Prelude[">>="]((dictMonadReader["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(ask(dictMonadReader))(function ($1) {
            return Prelude["return"]((dictMonadReader["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(f($1));
        });
    };
};
module.exports = {
    MonadReader: MonadReader, 
    reader: reader, 
    local: local, 
    ask: ask, 
    monadReaderFun: monadReaderFun
};

},{"../Prelude":203}],64:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_Plus = require("../Control.Plus");
var Data_Distributive = require("../Data.Distributive");
var Data_Either = require("../Data.Either");
var ReaderT = function (x) {
    return x;
};
var runReaderT = function (v) {
    return v;
};
var withReaderT = function (f) {
    return function (m) {
        return ReaderT(function ($27) {
            return runReaderT(m)(f($27));
        });
    };
};
var monadTransReaderT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function ($28) {
        return ReaderT(Prelude["const"]($28));
    };
});
var mapReaderT = function (f) {
    return function (m) {
        return ReaderT(function ($29) {
            return f(runReaderT(m)($29));
        });
    };
};
var functorReaderT = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        return mapReaderT(Prelude["<$>"](dictFunctor)(f));
    });
};
var distributiveReaderT = function (dictDistributive) {
    return new Data_Distributive.Distributive(function () {
        return functorReaderT(dictDistributive["__superclass_Prelude.Functor_0"]());
    }, function (dictFunctor) {
        return function (f) {
            return function ($30) {
                return Data_Distributive.distribute(distributiveReaderT(dictDistributive))(dictFunctor)(Prelude.map(dictFunctor)(f)($30));
            };
        };
    }, function (dictFunctor) {
        return function (a) {
            return function (e) {
                return Data_Distributive.collect(dictDistributive)(dictFunctor)(Prelude.flip(runReaderT)(e))(a);
            };
        };
    });
};
var applyReaderT = function (dictApplicative) {
    return new Prelude.Apply(function () {
        return functorReaderT((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
    }, function (f) {
        return function (v) {
            return function (r) {
                return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(runReaderT(f)(r))(runReaderT(v)(r));
            };
        };
    });
};
var bindReaderT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyReaderT(dictMonad["__superclass_Prelude.Applicative_0"]());
    }, function (m) {
        return function (k) {
            return function (r) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runReaderT(m)(r))(function (v) {
                    return runReaderT(k(v))(r);
                });
            };
        };
    });
};
var applicativeReaderT = function (dictApplicative) {
    return new Prelude.Applicative(function () {
        return applyReaderT(dictApplicative);
    }, function ($31) {
        return ReaderT(Prelude["const"](Prelude.pure(dictApplicative)($31)));
    });
};
var monadReaderT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeReaderT(dictMonad["__superclass_Prelude.Applicative_0"]());
    }, function () {
        return bindReaderT(dictMonad);
    });
};
var monadContReaderT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadReaderT(dictMonadCont["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return ReaderT(function (r) {
            return Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                return runReaderT(f(function (a) {
                    return ReaderT(Prelude["const"](c(a)));
                }))(r);
            });
        });
    });
};
var monadEffReader = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadReaderT(dictMonadEff["__superclass_Prelude.Monad_0"]());
    }, function ($32) {
        return Control_Monad_Trans.lift(monadTransReaderT)(dictMonadEff["__superclass_Prelude.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($32));
    });
};
var monadErrorReaderT = function (dictMonadError) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadReaderT(dictMonadError["__superclass_Prelude.Monad_0"]());
    }, function (m) {
        return function (h) {
            return ReaderT(function (r) {
                return Control_Monad_Error_Class.catchError(dictMonadError)(runReaderT(m)(r))(function (e) {
                    return runReaderT(h(e))(r);
                });
            });
        };
    }, function (e) {
        return Control_Monad_Trans.lift(monadTransReaderT)(dictMonadError["__superclass_Prelude.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
    });
};
var monadReaderReaderT = function (dictMonad) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadReaderT(dictMonad);
    }, Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]()), withReaderT);
};
var monadRecReaderT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadReaderT(dictMonadRec["__superclass_Prelude.Monad_0"]());
    }, function (k) {
        return function (a) {
            var k$prime = function (r) {
                return function (a1) {
                    return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runReaderT(k(a1))(r))(function (v) {
                        return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(Data_Either.either(Data_Either.Left.create)(Data_Either.Right.create)(v));
                    });
                };
            };
            return function (r) {
                return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(k$prime(r))(a);
            };
        };
    });
};
var monadStateReaderT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadReaderT(dictMonadState["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return Control_Monad_Trans.lift(monadTransReaderT)(dictMonadState["__superclass_Prelude.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
};
var monadWriterReaderT = function (dictMonad) {
    return function (dictMonadWriter) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadReaderT(dictMonad);
        }, mapReaderT(Control_Monad_Writer_Class.listen(dictMonadWriter)), mapReaderT(Control_Monad_Writer_Class.pass(dictMonadWriter)), function (wd) {
            return Control_Monad_Trans.lift(monadTransReaderT)(dictMonad)(Control_Monad_Writer_Class.writer(dictMonadWriter)(wd));
        });
    };
};
var altReaderT = function (dictAlt) {
    return new Control_Alt.Alt(function () {
        return functorReaderT(dictAlt["__superclass_Prelude.Functor_0"]());
    }, function (m) {
        return function (n) {
            return function (r) {
                return Control_Alt["<|>"](dictAlt)(runReaderT(m)(r))(runReaderT(n)(r));
            };
        };
    });
};
var plusReaderT = function (dictPlus) {
    return new Control_Plus.Plus(function () {
        return altReaderT(dictPlus["__superclass_Control.Alt.Alt_0"]());
    }, Prelude["const"](Control_Plus.empty(dictPlus)));
};
var alternativeReaderT = function (dictAlternative) {
    return new Control_Alternative.Alternative(function () {
        return plusReaderT(dictAlternative["__superclass_Control.Plus.Plus_1"]());
    }, function () {
        return applicativeReaderT(dictAlternative["__superclass_Prelude.Applicative_0"]());
    });
};
var monadPlusReaderT = function (dictMonadPlus) {
    return new Control_MonadPlus.MonadPlus(function () {
        return alternativeReaderT(dictMonadPlus["__superclass_Control.Alternative.Alternative_1"]());
    }, function () {
        return monadReaderT(dictMonadPlus["__superclass_Prelude.Monad_0"]());
    });
};
module.exports = {
    ReaderT: ReaderT, 
    mapReaderT: mapReaderT, 
    withReaderT: withReaderT, 
    runReaderT: runReaderT, 
    functorReaderT: functorReaderT, 
    applyReaderT: applyReaderT, 
    applicativeReaderT: applicativeReaderT, 
    altReaderT: altReaderT, 
    plusReaderT: plusReaderT, 
    alternativeReaderT: alternativeReaderT, 
    bindReaderT: bindReaderT, 
    monadReaderT: monadReaderT, 
    monadPlusReaderT: monadPlusReaderT, 
    monadTransReaderT: monadTransReaderT, 
    monadEffReader: monadEffReader, 
    monadContReaderT: monadContReaderT, 
    monadErrorReaderT: monadErrorReaderT, 
    monadReaderReaderT: monadReaderReaderT, 
    monadStateReaderT: monadStateReaderT, 
    monadWriterReaderT: monadWriterReaderT, 
    distributiveReaderT: distributiveReaderT, 
    monadRecReaderT: monadRecReaderT
};

},{"../Control.Alt":26,"../Control.Alternative":27,"../Control.Monad.Cont.Class":44,"../Control.Monad.Eff.Class":46,"../Control.Monad.Error.Class":55,"../Control.Monad.Reader.Class":63,"../Control.Monad.Rec.Class":65,"../Control.Monad.State.Class":68,"../Control.Monad.Trans":71,"../Control.Monad.Writer.Class":72,"../Control.MonadPlus":76,"../Control.Plus":77,"../Data.Distributive":111,"../Data.Either":114,"../Prelude":203}],65:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_ST = require("../Control.Monad.ST");
var Data_Either = require("../Data.Either");
var Data_Functor = require("../Data.Functor");
var Data_Identity = require("../Data.Identity");
var Control_Monad_Eff_Unsafe = require("../Control.Monad.Eff.Unsafe");
var Data_Either_Unsafe = require("../Data.Either.Unsafe");
var MonadRec = function (__superclass_Prelude$dotMonad_0, tailRecM) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.tailRecM = tailRecM;
};
var tailRecM = function (dict) {
    return dict.tailRecM;
};
var tailRecM2 = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (b) {
                return tailRecM(dictMonadRec)(function (o) {
                    return f(o.a)(o.b);
                })({
                    a: a, 
                    b: b
                });
            };
        };
    };
};
var tailRecM3 = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return tailRecM(dictMonadRec)(function (o) {
                        return f(o.a)(o.b)(o.c);
                    })({
                        a: a, 
                        b: b, 
                        c: c
                    });
                };
            };
        };
    };
};
var tailRecEff = function (f) {
    return function (a) {
        var f$prime = function ($18) {
            return Control_Monad_Eff_Unsafe.unsafeInterleaveEff(f($18));
        };
        return function __do() {
            var v = f$prime(a)();
            var v1 = {
                value: v
            };
            (function () {
                while (!(function __do() {
                    var v2 = v1.value;
                    if (v2 instanceof Data_Either.Left) {
                        var v3 = f$prime(v2.value0)();
                        v1.value = v3;
                        return false;
                    };
                    if (v2 instanceof Data_Either.Right) {
                        return true;
                    };
                    throw new Error("Failed pattern match at Control.Monad.Rec.Class line 81, column 5 - line 86, column 3: " + [ v2.constructor.name ]);
                })()) {

                };
                return {};
            })();
            return Prelude["<$>"](Control_Monad_Eff.functorEff)(Data_Either_Unsafe.fromRight)(Control_Monad_ST.readSTRef(v1))();
        };
    };
};
var tailRec = function (f) {
    return function (a) {
        var go = function (__copy_v) {
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Data_Either.Left) {
                    var __tco_v = f(v.value0);
                    v = __tco_v;
                    continue tco;
                };
                if (v instanceof Data_Either.Right) {
                    return v.value0;
                };
                throw new Error("Failed pattern match at Control.Monad.Rec.Class line 64, column 1 - line 69, column 1: " + [ v.constructor.name ]);
            };
        };
        return go(f(a));
    };
};
var monadRecIdentity = new MonadRec(function () {
    return Data_Identity.monadIdentity;
}, function (f) {
    return function ($19) {
        return Data_Identity.Identity(tailRec(function ($20) {
            return Data_Identity.runIdentity(f($20));
        })($19));
    };
});
var monadRecEff = new MonadRec(function () {
    return Control_Monad_Eff.monadEff;
}, tailRecEff);
var forever = function (dictMonadRec) {
    return function (ma) {
        return tailRecM(dictMonadRec)(function (u) {
            return Data_Functor["<$"]((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(new Data_Either.Left(u))(ma);
        })(Prelude.unit);
    };
};
module.exports = {
    MonadRec: MonadRec, 
    forever: forever, 
    tailRecM3: tailRecM3, 
    tailRecM2: tailRecM2, 
    tailRecM: tailRecM, 
    tailRec: tailRec, 
    monadRecIdentity: monadRecIdentity, 
    monadRecEff: monadRecEff
};

},{"../Control.Monad.Eff":54,"../Control.Monad.Eff.Unsafe":52,"../Control.Monad.ST":67,"../Data.Either":114,"../Data.Either.Unsafe":113,"../Data.Functor":134,"../Data.Identity":137,"../Prelude":203}],66:[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.ST

exports.newSTRef = function (val) {
  return function () {
    return { value: val };
  };
};

exports.readSTRef = function (ref) {
  return function () {
    return ref.value;
  };
};

exports.modifySTRef = function (ref) {
  return function (f) {
    return function () {
      /* jshint boss: true */
      return ref.value = f(ref.value);
    };
  };
};

exports.writeSTRef = function (ref) {
  return function (a) {
    return function () {
      /* jshint boss: true */
      return ref.value = a;
    };
  };
};

exports.runST = function (f) {
  return f;
};

},{}],67:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var pureST = function (st) {
    return Control_Monad_Eff.runPure($foreign.runST(st));
};
module.exports = {
    pureST: pureST, 
    runST: $foreign.runST, 
    writeSTRef: $foreign.writeSTRef, 
    modifySTRef: $foreign.modifySTRef, 
    readSTRef: $foreign.readSTRef, 
    newSTRef: $foreign.newSTRef
};

},{"../Control.Monad.Eff":54,"../Prelude":203,"./foreign":66}],68:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Tuple = require("../Data.Tuple");
var MonadState = function (__superclass_Prelude$dotMonad_0, state) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.state = state;
};
var state = function (dict) {
    return dict.state;
};
var put = function (dictMonadState) {
    return function (s) {
        return state(dictMonadState)(function (v) {
            return new Data_Tuple.Tuple(Prelude.unit, s);
        });
    };
};
var modify = function (dictMonadState) {
    return function (f) {
        return state(dictMonadState)(function (s) {
            return new Data_Tuple.Tuple(Prelude.unit, f(s));
        });
    };
};
var gets = function (dictMonadState) {
    return function (f) {
        return state(dictMonadState)(function (s) {
            return new Data_Tuple.Tuple(f(s), s);
        });
    };
};
var get = function (dictMonadState) {
    return state(dictMonadState)(function (s) {
        return new Data_Tuple.Tuple(s, s);
    });
};
module.exports = {
    MonadState: MonadState, 
    modify: modify, 
    put: put, 
    gets: gets, 
    get: get, 
    state: state
};

},{"../Data.Tuple":171,"../Prelude":203}],69:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Tuple = require("../Data.Tuple");
var Data_Either = require("../Data.Either");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Lazy = require("../Control.Lazy");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_Plus = require("../Control.Plus");
var StateT = function (x) {
    return x;
};
var runStateT = function (v) {
    return v;
};
var withStateT = function (f) {
    return function (s) {
        return StateT(function ($60) {
            return runStateT(s)(f($60));
        });
    };
};
var monadTransStateT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function (m) {
        return function (s) {
            return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v, s));
            });
        };
    };
});
var mapStateT = function (f) {
    return function (m) {
        return StateT(function ($61) {
            return f(runStateT(m)($61));
        });
    };
};
var lazyStateT = new Control_Lazy.Lazy(function (f) {
    return StateT(function (s) {
        return runStateT(f(Prelude.unit))(s);
    });
});
var execStateT = function (dictApply) {
    return function (m) {
        return function (s) {
            return Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Data_Tuple.snd)(runStateT(m)(s));
        };
    };
};
var evalStateT = function (dictApply) {
    return function (m) {
        return function (s) {
            return Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Data_Tuple.fst)(runStateT(m)(s));
        };
    };
};
var monadStateT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeStateT(dictMonad);
    }, function () {
        return bindStateT(dictMonad);
    });
};
var functorStateT = function (dictMonad) {
    return new Prelude.Functor(Prelude.liftM1(monadStateT(dictMonad)));
};
var bindStateT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyStateT(dictMonad);
    }, function (v) {
        return function (f) {
            return function (s) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(v(s))(function (v1) {
                    return runStateT(f(v1.value0))(v1.value1);
                });
            };
        };
    });
};
var applyStateT = function (dictMonad) {
    return new Prelude.Apply(function () {
        return functorStateT(dictMonad);
    }, Prelude.ap(monadStateT(dictMonad)));
};
var applicativeStateT = function (dictMonad) {
    return new Prelude.Applicative(function () {
        return applyStateT(dictMonad);
    }, function (a) {
        return StateT(function (s) {
            return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(a, s));
        });
    });
};
var monadContStateT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadStateT(dictMonadCont["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return StateT(function (s) {
            return Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                return runStateT(f(function (a) {
                    return StateT(function (s$prime) {
                        return c(new Data_Tuple.Tuple(a, s$prime));
                    });
                }))(s);
            });
        });
    });
};
var monadEffState = function (dictMonad) {
    return function (dictMonadEff) {
        return new Control_Monad_Eff_Class.MonadEff(function () {
            return monadStateT(dictMonad);
        }, function ($62) {
            return Control_Monad_Trans.lift(monadTransStateT)(dictMonad)(Control_Monad_Eff_Class.liftEff(dictMonadEff)($62));
        });
    };
};
var monadErrorStateT = function (dictMonadError) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadStateT(dictMonadError["__superclass_Prelude.Monad_0"]());
    }, function (m) {
        return function (h) {
            return StateT(function (s) {
                return Control_Monad_Error_Class.catchError(dictMonadError)(runStateT(m)(s))(function (e) {
                    return runStateT(h(e))(s);
                });
            });
        };
    }, function (e) {
        return Control_Monad_Trans.lift(monadTransStateT)(dictMonadError["__superclass_Prelude.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
    });
};
var monadReaderStateT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadStateT(dictMonadReader["__superclass_Prelude.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransStateT)(dictMonadReader["__superclass_Prelude.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return mapStateT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
    });
};
var monadRecStateT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadStateT(dictMonadRec["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return function (a) {
            var f$prime = function (v) {
                return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runStateT(f(v.value0))(v.value1))(function (v1) {
                    return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                        if (v1.value0 instanceof Data_Either.Left) {
                            return new Data_Either.Left(new Data_Tuple.Tuple(v1.value0.value0, v1.value1));
                        };
                        if (v1.value0 instanceof Data_Either.Right) {
                            return new Data_Either.Right(new Data_Tuple.Tuple(v1.value0.value0, v1.value1));
                        };
                        throw new Error("Failed pattern match at Control.Monad.State.Trans line 85, column 7 - line 89, column 1: " + [ v1.value0.constructor.name ]);
                    })());
                });
            };
            return function (s) {
                return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(f$prime)(new Data_Tuple.Tuple(a, s));
            };
        };
    });
};
var monadStateStateT = function (dictMonad) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadStateT(dictMonad);
    }, function (f) {
        return StateT(function ($63) {
            return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(f($63));
        });
    });
};
var monadWriterStateT = function (dictMonad) {
    return function (dictMonadWriter) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadStateT(dictMonad);
        }, function (m) {
            return StateT(function (s) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(Control_Monad_Writer_Class.listen(dictMonadWriter)(runStateT(m)(s)))(function (v) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
                });
            });
        }, function (m) {
            return StateT(function (s) {
                return Control_Monad_Writer_Class.pass(dictMonadWriter)(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runStateT(m)(s))(function (v) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
                }));
            });
        }, function (wd) {
            return Control_Monad_Trans.lift(monadTransStateT)(dictMonad)(Control_Monad_Writer_Class.writer(dictMonadWriter)(wd));
        });
    };
};
var altStateT = function (dictMonad) {
    return function (dictAlt) {
        return new Control_Alt.Alt(function () {
            return functorStateT(dictMonad);
        }, function (x) {
            return function (y) {
                return StateT(function (s) {
                    return Control_Alt["<|>"](dictAlt)(runStateT(x)(s))(runStateT(y)(s));
                });
            };
        });
    };
};
var plusStateT = function (dictMonad) {
    return function (dictPlus) {
        return new Control_Plus.Plus(function () {
            return altStateT(dictMonad)(dictPlus["__superclass_Control.Alt.Alt_0"]());
        }, StateT(function (v) {
            return Control_Plus.empty(dictPlus);
        }));
    };
};
var alternativeStateT = function (dictMonad) {
    return function (dictAlternative) {
        return new Control_Alternative.Alternative(function () {
            return plusStateT(dictMonad)(dictAlternative["__superclass_Control.Plus.Plus_1"]());
        }, function () {
            return applicativeStateT(dictMonad);
        });
    };
};
var monadPlusStateT = function (dictMonadPlus) {
    return new Control_MonadPlus.MonadPlus(function () {
        return alternativeStateT(dictMonadPlus["__superclass_Prelude.Monad_0"]())(dictMonadPlus["__superclass_Control.Alternative.Alternative_1"]());
    }, function () {
        return monadStateT(dictMonadPlus["__superclass_Prelude.Monad_0"]());
    });
};
module.exports = {
    StateT: StateT, 
    withStateT: withStateT, 
    mapStateT: mapStateT, 
    execStateT: execStateT, 
    evalStateT: evalStateT, 
    runStateT: runStateT, 
    functorStateT: functorStateT, 
    applyStateT: applyStateT, 
    applicativeStateT: applicativeStateT, 
    altStateT: altStateT, 
    plusStateT: plusStateT, 
    alternativeStateT: alternativeStateT, 
    bindStateT: bindStateT, 
    monadStateT: monadStateT, 
    monadRecStateT: monadRecStateT, 
    monadPlusStateT: monadPlusStateT, 
    monadTransStateT: monadTransStateT, 
    lazyStateT: lazyStateT, 
    monadEffState: monadEffState, 
    monadContStateT: monadContStateT, 
    monadErrorStateT: monadErrorStateT, 
    monadReaderStateT: monadReaderStateT, 
    monadStateStateT: monadStateStateT, 
    monadWriterStateT: monadWriterStateT
};

},{"../Control.Alt":26,"../Control.Alternative":27,"../Control.Lazy":37,"../Control.Monad.Cont.Class":44,"../Control.Monad.Eff.Class":46,"../Control.Monad.Error.Class":55,"../Control.Monad.Reader.Class":63,"../Control.Monad.Rec.Class":65,"../Control.Monad.State.Class":68,"../Control.Monad.Trans":71,"../Control.Monad.Writer.Class":72,"../Control.MonadPlus":76,"../Control.Plus":77,"../Data.Either":114,"../Data.Tuple":171,"../Prelude":203}],70:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Data_Identity = require("../Data.Identity");
var Data_Tuple = require("../Data.Tuple");
var withState = Control_Monad_State_Trans.withStateT;
var runState = function (s) {
    return function ($0) {
        return Data_Identity.runIdentity(Control_Monad_State_Trans.runStateT(s)($0));
    };
};
var mapState = function (f) {
    return Control_Monad_State_Trans.mapStateT(function ($1) {
        return Data_Identity.Identity(f(Data_Identity.runIdentity($1)));
    });
};
var execState = function (m) {
    return function (s) {
        return Data_Tuple.snd(runState(m)(s));
    };
};
var evalState = function (m) {
    return function (s) {
        return Data_Tuple.fst(runState(m)(s));
    };
};
module.exports = {
    withState: withState, 
    mapState: mapState, 
    execState: execState, 
    evalState: evalState, 
    runState: runState
};

},{"../Control.Monad.State.Class":68,"../Control.Monad.State.Trans":69,"../Data.Identity":137,"../Data.Tuple":171,"../Prelude":203}],71:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var MonadTrans = function (lift) {
    this.lift = lift;
};
var lift = function (dict) {
    return dict.lift;
};
module.exports = {
    MonadTrans: MonadTrans, 
    lift: lift
};

},{"../Prelude":203}],72:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Monoid = require("../Data.Monoid");
var Data_Tuple = require("../Data.Tuple");
var MonadWriter = function (__superclass_Prelude$dotMonad_0, listen, pass, writer) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.listen = listen;
    this.pass = pass;
    this.writer = writer;
};
var writer = function (dict) {
    return dict.writer;
};
var tell = function (dictMonoid) {
    return function (dictMonad) {
        return function (dictMonadWriter) {
            return function (w) {
                return writer(dictMonadWriter)(new Data_Tuple.Tuple(Prelude.unit, w));
            };
        };
    };
};
var pass = function (dict) {
    return dict.pass;
};
var listen = function (dict) {
    return dict.listen;
};
var listens = function (dictMonoid) {
    return function (dictMonad) {
        return function (dictMonadWriter) {
            return function (f) {
                return function (m) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(listen(dictMonadWriter)(m))(function (v) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v.value0, f(v.value1)));
                    });
                };
            };
        };
    };
};
var censor = function (dictMonoid) {
    return function (dictMonad) {
        return function (dictMonadWriter) {
            return function (f) {
                return function (m) {
                    return pass(dictMonadWriter)(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v, f));
                    }));
                };
            };
        };
    };
};
module.exports = {
    MonadWriter: MonadWriter, 
    censor: censor, 
    listens: listens, 
    tell: tell, 
    pass: pass, 
    listen: listen, 
    writer: writer
};

},{"../Data.Monoid":159,"../Data.Tuple":171,"../Prelude":203}],73:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Monoid = require("../Data.Monoid");
var Data_Tuple = require("../Data.Tuple");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_Plus = require("../Control.Plus");
var WriterT = function (x) {
    return x;
};
var runWriterT = function (v) {
    return v;
};
var monadTransWriterT = function (dictMonoid) {
    return new Control_Monad_Trans.MonadTrans(function (dictMonad) {
        return function (m) {
            return WriterT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v, Data_Monoid.mempty(dictMonoid)));
            }));
        };
    });
};
var mapWriterT = function (f) {
    return function (m) {
        return WriterT(f(runWriterT(m)));
    };
};
var functorWriterT = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        return mapWriterT(Prelude["<$>"](dictFunctor)(function (v) {
            return new Data_Tuple.Tuple(f(v.value0), v.value1);
        }));
    });
};
var execWriterT = function (dictApply) {
    return function (m) {
        return Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Data_Tuple.snd)(runWriterT(m));
    };
};
var applyWriterT = function (dictSemigroup) {
    return function (dictApply) {
        return new Prelude.Apply(function () {
            return functorWriterT(dictApply["__superclass_Prelude.Functor_0"]());
        }, function (f) {
            return function (v) {
                return WriterT((function () {
                    var k = function (v1) {
                        return function (v2) {
                            return new Data_Tuple.Tuple(v1.value0(v2.value0), Prelude["<>"](dictSemigroup)(v1.value1)(v2.value1));
                        };
                    };
                    return Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(k)(runWriterT(f)))(runWriterT(v));
                })());
            };
        });
    };
};
var bindWriterT = function (dictSemigroup) {
    return function (dictMonad) {
        return new Prelude.Bind(function () {
            return applyWriterT(dictSemigroup)((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]());
        }, function (m) {
            return function (k) {
                return WriterT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runWriterT(m))(function (v) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runWriterT(k(v.value0)))(function (v1) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v1.value0, Prelude["<>"](dictSemigroup)(v.value1)(v1.value1)));
                    });
                }));
            };
        });
    };
};
var applicativeWriterT = function (dictMonoid) {
    return function (dictApplicative) {
        return new Prelude.Applicative(function () {
            return applyWriterT(dictMonoid["__superclass_Prelude.Semigroup_0"]())(dictApplicative["__superclass_Prelude.Apply_0"]());
        }, function (a) {
            return WriterT(Prelude.pure(dictApplicative)(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
        });
    };
};
var monadWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return new Prelude.Monad(function () {
            return applicativeWriterT(dictMonoid)(dictMonad["__superclass_Prelude.Applicative_0"]());
        }, function () {
            return bindWriterT(dictMonoid["__superclass_Prelude.Semigroup_0"]())(dictMonad);
        });
    };
};
var monadContWriterT = function (dictMonoid) {
    return function (dictMonadCont) {
        return new Control_Monad_Cont_Class.MonadCont(function () {
            return monadWriterT(dictMonoid)(dictMonadCont["__superclass_Prelude.Monad_0"]());
        }, function (f) {
            return WriterT(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                return runWriterT(f(function (a) {
                    return WriterT(c(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
                }));
            }));
        });
    };
};
var monadEffWriter = function (dictMonad) {
    return function (dictMonoid) {
        return function (dictMonadEff) {
            return new Control_Monad_Eff_Class.MonadEff(function () {
                return monadWriterT(dictMonoid)(dictMonad);
            }, function ($78) {
                return Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonad)(Control_Monad_Eff_Class.liftEff(dictMonadEff)($78));
            });
        };
    };
};
var monadErrorWriterT = function (dictMonoid) {
    return function (dictMonadError) {
        return new Control_Monad_Error_Class.MonadError(function () {
            return monadWriterT(dictMonoid)(dictMonadError["__superclass_Prelude.Monad_0"]());
        }, function (m) {
            return function (h) {
                return WriterT(Control_Monad_Error_Class.catchError(dictMonadError)(runWriterT(m))(function (e) {
                    return runWriterT(h(e));
                }));
            };
        }, function (e) {
            return Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonadError["__superclass_Prelude.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
        });
    };
};
var monadReaderWriterT = function (dictMonoid) {
    return function (dictMonadReader) {
        return new Control_Monad_Reader_Class.MonadReader(function () {
            return monadWriterT(dictMonoid)(dictMonadReader["__superclass_Prelude.Monad_0"]());
        }, Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonadReader["__superclass_Prelude.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
            return mapWriterT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
        });
    };
};
var monadRecWriterT = function (dictMonoid) {
    return function (dictMonadRec) {
        return new Control_Monad_Rec_Class.MonadRec(function () {
            return monadWriterT(dictMonoid)(dictMonadRec["__superclass_Prelude.Monad_0"]());
        }, function (f) {
            return function (a) {
                var f$prime = function (v) {
                    return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runWriterT(f(v.value0)))(function (v1) {
                        return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                            if (v1.value0 instanceof Data_Either.Left) {
                                return new Data_Either.Left(new Data_Tuple.Tuple(v1.value0.value0, Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value1)(v1.value1)));
                            };
                            if (v1.value0 instanceof Data_Either.Right) {
                                return new Data_Either.Right(new Data_Tuple.Tuple(v1.value0.value0, Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value1)(v1.value1)));
                            };
                            throw new Error("Failed pattern match at Control.Monad.Writer.Trans line 80, column 7 - line 84, column 1: " + [ v1.value0.constructor.name ]);
                        })());
                    });
                };
                return WriterT(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(f$prime)(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
            };
        });
    };
};
var monadStateWriterT = function (dictMonoid) {
    return function (dictMonadState) {
        return new Control_Monad_State_Class.MonadState(function () {
            return monadWriterT(dictMonoid)(dictMonadState["__superclass_Prelude.Monad_0"]());
        }, function (f) {
            return Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonadState["__superclass_Prelude.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)(f));
        });
    };
};
var monadWriterWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadWriterT(dictMonoid)(dictMonad);
        }, function (m) {
            return WriterT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runWriterT(m))(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0, v.value1), v.value1));
            }));
        }, function (m) {
            return WriterT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runWriterT(m))(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v.value0.value0, v.value0.value1(v.value1)));
            }));
        }, function ($79) {
            return WriterT(Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())($79));
        });
    };
};
var altWriterT = function (dictAlt) {
    return new Control_Alt.Alt(function () {
        return functorWriterT(dictAlt["__superclass_Prelude.Functor_0"]());
    }, function (m) {
        return function (n) {
            return WriterT(Control_Alt["<|>"](dictAlt)(runWriterT(m))(runWriterT(n)));
        };
    });
};
var plusWriterT = function (dictPlus) {
    return new Control_Plus.Plus(function () {
        return altWriterT(dictPlus["__superclass_Control.Alt.Alt_0"]());
    }, Control_Plus.empty(dictPlus));
};
var alternativeWriterT = function (dictMonoid) {
    return function (dictAlternative) {
        return new Control_Alternative.Alternative(function () {
            return plusWriterT(dictAlternative["__superclass_Control.Plus.Plus_1"]());
        }, function () {
            return applicativeWriterT(dictMonoid)(dictAlternative["__superclass_Prelude.Applicative_0"]());
        });
    };
};
var monadPlusWriterT = function (dictMonoid) {
    return function (dictMonadPlus) {
        return new Control_MonadPlus.MonadPlus(function () {
            return alternativeWriterT(dictMonoid)(dictMonadPlus["__superclass_Control.Alternative.Alternative_1"]());
        }, function () {
            return monadWriterT(dictMonoid)(dictMonadPlus["__superclass_Prelude.Monad_0"]());
        });
    };
};
module.exports = {
    WriterT: WriterT, 
    mapWriterT: mapWriterT, 
    execWriterT: execWriterT, 
    runWriterT: runWriterT, 
    functorWriterT: functorWriterT, 
    applyWriterT: applyWriterT, 
    applicativeWriterT: applicativeWriterT, 
    altWriterT: altWriterT, 
    plusWriterT: plusWriterT, 
    alternativeWriterT: alternativeWriterT, 
    bindWriterT: bindWriterT, 
    monadWriterT: monadWriterT, 
    monadRecWriterT: monadRecWriterT, 
    monadPlusWriterT: monadPlusWriterT, 
    monadTransWriterT: monadTransWriterT, 
    monadEffWriter: monadEffWriter, 
    monadContWriterT: monadContWriterT, 
    monadErrorWriterT: monadErrorWriterT, 
    monadReaderWriterT: monadReaderWriterT, 
    monadStateWriterT: monadStateWriterT, 
    monadWriterWriterT: monadWriterWriterT
};

},{"../Control.Alt":26,"../Control.Alternative":27,"../Control.Monad.Cont.Class":44,"../Control.Monad.Eff.Class":46,"../Control.Monad.Error.Class":55,"../Control.Monad.Reader.Class":63,"../Control.Monad.Rec.Class":65,"../Control.Monad.State.Class":68,"../Control.Monad.Trans":71,"../Control.Monad.Writer.Class":72,"../Control.MonadPlus":76,"../Control.Plus":77,"../Data.Either":114,"../Data.Monoid":159,"../Data.Tuple":171,"../Prelude":203}],74:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans");
var Data_Identity = require("../Data.Identity");
var Data_Tuple = require("../Data.Tuple");
var runWriter = function ($0) {
    return Data_Identity.runIdentity(Control_Monad_Writer_Trans.runWriterT($0));
};
var mapWriter = function (f) {
    return Control_Monad_Writer_Trans.mapWriterT(function ($1) {
        return Data_Identity.Identity(f(Data_Identity.runIdentity($1)));
    });
};
var execWriter = function (m) {
    return Data_Tuple.snd(runWriter(m));
};
module.exports = {
    mapWriter: mapWriter, 
    execWriter: execWriter, 
    runWriter: runWriter
};

},{"../Control.Monad.Writer.Class":72,"../Control.Monad.Writer.Trans":73,"../Data.Identity":137,"../Data.Tuple":171,"../Prelude":203}],75:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var when = function (dictMonad) {
    return function (v) {
        return function (v1) {
            if (v) {
                return v1;
            };
            if (!v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Prelude.unit);
            };
            throw new Error("Failed pattern match at Control.Monad line 9, column 1 - line 10, column 1: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var unless = function (dictMonad) {
    return function (v) {
        return function (v1) {
            if (!v) {
                return v1;
            };
            if (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Prelude.unit);
            };
            throw new Error("Failed pattern match at Control.Monad line 14, column 1 - line 15, column 1: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
module.exports = {
    unless: unless, 
    when: when
};

},{"../Prelude":203}],76:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Alternative = require("../Control.Alternative");
var Control_Plus = require("../Control.Plus");
var MonadPlus = function (__superclass_Control$dotAlternative$dotAlternative_1, __superclass_Prelude$dotMonad_0) {
    this["__superclass_Control.Alternative.Alternative_1"] = __superclass_Control$dotAlternative$dotAlternative_1;
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
};
var monadPlusArray = new MonadPlus(function () {
    return Control_Alternative.alternativeArray;
}, function () {
    return Prelude.monadArray;
});
var guard = function (dictMonadPlus) {
    return function (v) {
        if (v) {
            return Prelude["return"]((dictMonadPlus["__superclass_Control.Alternative.Alternative_1"]())["__superclass_Prelude.Applicative_0"]())(Prelude.unit);
        };
        if (!v) {
            return Control_Plus.empty((dictMonadPlus["__superclass_Control.Alternative.Alternative_1"]())["__superclass_Control.Plus.Plus_1"]());
        };
        throw new Error("Failed pattern match at Control.MonadPlus line 36, column 1 - line 37, column 1: " + [ v.constructor.name ]);
    };
};
module.exports = {
    MonadPlus: MonadPlus, 
    guard: guard, 
    monadPlusArray: monadPlusArray
};

},{"../Control.Alternative":27,"../Control.Plus":77,"../Prelude":203}],77:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Plus = function (__superclass_Control$dotAlt$dotAlt_0, empty) {
    this["__superclass_Control.Alt.Alt_0"] = __superclass_Control$dotAlt$dotAlt_0;
    this.empty = empty;
};
var plusArray = new Plus(function () {
    return Control_Alt.altArray;
}, [  ]);
var empty = function (dict) {
    return dict.empty;
};
module.exports = {
    Plus: Plus, 
    empty: empty, 
    plusArray: plusArray
};

},{"../Control.Alt":26,"../Prelude":203}],78:[function(require,module,exports){
"use strict";

// module DOM.Event.DragEvent.DataTransfer

exports.files = function (dataTransfer) {
  return dataTransfer.files;
};

},{}],79:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var DOM_File_Types = require("../DOM.File.Types");
module.exports = {
    files: $foreign.files
};

},{"../DOM.File.Types":85,"./foreign":78}],80:[function(require,module,exports){
"use strict";

// module DOM.Event.EventTarget

exports.eventListener = function (fn) {
  return function (event) {
    return fn(event)();
  };
};

exports.addEventListener = function (type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          target.addEventListener(type, listener, useCapture);
          return {};
        };
      };
    };
  };
};

exports.removeEventListener = function (type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          target.removeEventListener(type, listener, useCapture);
          return {};
        };
      };
    };
  };
};

exports.dispatchEvent = function (event) {
  return function (target) {
    return function () {
      return target.dispatchEvent(event);
    };
  };
};

},{}],81:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var DOM = require("../DOM");
var DOM_Event_Types = require("../DOM.Event.Types");
module.exports = {
    dispatchEvent: $foreign.dispatchEvent, 
    removeEventListener: $foreign.removeEventListener, 
    addEventListener: $foreign.addEventListener, 
    eventListener: $foreign.eventListener
};

},{"../Control.Monad.Eff":54,"../Control.Monad.Eff.Exception":48,"../DOM":98,"../DOM.Event.Types":84,"../Prelude":203,"./foreign":80}],82:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var DOM_Event_Types = require("../DOM.Event.Types");
var wheel = "wheel";
var waiting = "waiting";
var volumechange = "volumechange";
var visibilitychange = "visibilitychange";
var unload = "unload";
var transitionend = "transitionend";
var touchstart = "touchstart";
var touchmove = "touchmove";
var touchleave = "touchleave";
var touchenter = "touchenter";
var touchend = "touchend";
var touchcancel = "touchcancel";
var timeupdate = "timeupdate";
var timeout = "timeout";
var suspend = "suspend";
var submit = "submit";
var stalled = "stalled";
var show = "show";
var select = "select";
var seeking = "seeking";
var seeked = "seeked";
var scroll = "scroll";
var resize = "resize";
var reset = "reset";
var readystatechange = "readystatechange";
var ratechange = "ratechange";
var progress = "progress";
var popstate = "popstate";
var playing = "playing";
var play = "play";
var pause = "pause";
var paste = "paste";
var pageshow = "pageshow";
var pagehide = "pagehide";
var open = "open";
var mouseup = "mouseup";
var mouseover = "mouseover";
var mouseout = "mouseout";
var mousemove = "mousemove";
var mouseleave = "mouseleave";
var mouseenter = "mouseenter";
var mousedown = "mousedown";
var message = "message";
var loadstart = "loadstart";
var loadend = "loadend";
var loadedmetadata = "loadedmetadata";
var loadeddata = "loadeddata";
var load = "load";
var keyup = "keyup";
var keypress = "keypress";
var keydown = "keydown";
var invalid = "invalid";
var input = "input";
var hashchange = "hashchange";
var fullscreenerror = "fullscreenerror";
var fullscreenchange = "fullscreenchange";
var focus = "focus";
var error = "error";
var ended = "ended";
var emptied = "emptied";
var durationchange = "durationchange";
var drop = "drop";
var dragstart = "dragstart";
var dragover = "dragover";
var dragleave = "dragleave";
var dragenter = "dragenter";
var dragend = "dragend";
var drag = "drag";
var dblclick = "dblclick";
var cut = "cut";
var copy = "copy";
var contextmenu = "contextmenu";
var compositionupdate = "compositionupdate";
var compositionstart = "compositionstart";
var compositionend = "compositionend";
var complete = "complete";
var click = "click";
var change = "change";
var canplaythrough = "canplaythrough";
var canplay = "canplay";
var blur = "blur";
var beforeunload = "beforeunload";
var beforeprint = "beforeprint";
var audioprocess = "audioprocess";
var animationstart = "animationstart";
var animationiteration = "animationiteration";
var animationend = "animationend";
var abort = "abort";
module.exports = {
    wheel: wheel, 
    waiting: waiting, 
    volumechange: volumechange, 
    visibilitychange: visibilitychange, 
    unload: unload, 
    transitionend: transitionend, 
    touchstart: touchstart, 
    touchmove: touchmove, 
    touchleave: touchleave, 
    touchenter: touchenter, 
    touchend: touchend, 
    touchcancel: touchcancel, 
    timeupdate: timeupdate, 
    timeout: timeout, 
    suspend: suspend, 
    submit: submit, 
    stalled: stalled, 
    show: show, 
    select: select, 
    seeking: seeking, 
    seeked: seeked, 
    scroll: scroll, 
    resize: resize, 
    reset: reset, 
    readystatechange: readystatechange, 
    ratechange: ratechange, 
    progress: progress, 
    popstate: popstate, 
    playing: playing, 
    play: play, 
    pause: pause, 
    paste: paste, 
    pageshow: pageshow, 
    pagehide: pagehide, 
    open: open, 
    mouseup: mouseup, 
    mouseover: mouseover, 
    mouseout: mouseout, 
    mousemove: mousemove, 
    mouseleave: mouseleave, 
    mouseenter: mouseenter, 
    mousedown: mousedown, 
    message: message, 
    loadstart: loadstart, 
    loadend: loadend, 
    loadedmetadata: loadedmetadata, 
    loadeddata: loadeddata, 
    load: load, 
    keyup: keyup, 
    keypress: keypress, 
    keydown: keydown, 
    invalid: invalid, 
    input: input, 
    hashchange: hashchange, 
    fullscreenerror: fullscreenerror, 
    fullscreenchange: fullscreenchange, 
    focus: focus, 
    error: error, 
    ended: ended, 
    emptied: emptied, 
    durationchange: durationchange, 
    drop: drop, 
    dragstart: dragstart, 
    dragover: dragover, 
    dragleave: dragleave, 
    dragenter: dragenter, 
    dragend: dragend, 
    drag: drag, 
    dblclick: dblclick, 
    cut: cut, 
    copy: copy, 
    contextmenu: contextmenu, 
    compositionupdate: compositionupdate, 
    compositionstart: compositionstart, 
    compositionend: compositionend, 
    complete: complete, 
    click: click, 
    change: change, 
    canplaythrough: canplaythrough, 
    canplay: canplay, 
    blur: blur, 
    beforeunload: beforeunload, 
    beforeprint: beforeprint, 
    audioprocess: audioprocess, 
    animationstart: animationstart, 
    animationiteration: animationiteration, 
    animationend: animationend, 
    abort: abort
};

},{"../DOM.Event.Types":84}],83:[function(require,module,exports){
/* global EventTarget */
"use strict";

// module DOM.Event.Types

exports._readEventTarget = function (left) {
  return function (right) {
    return function (foreign) {
      return foreign instanceof EventTarget ? left("Value is not an EventTarget") : right(foreign);
    };
  };
};

},{}],84:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Foreign_Class = require("../Data.Foreign.Class");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var EventType = function (x) {
    return x;
};
var unsafeToEvent = Unsafe_Coerce.unsafeCoerce;
var userProximityEventToEvent = unsafeToEvent;
var wheelEventToEvent = unsafeToEvent;
var uiEventToEvent = unsafeToEvent;
var transitionEventToEvent = unsafeToEvent;
var touchEventToEvent = unsafeToEvent;
var timeEventToEvent = unsafeToEvent;
var svgZoomEventToEvent = unsafeToEvent;
var svgEventToEvent = unsafeToEvent;
var storageEventToEvent = unsafeToEvent;
var sensorEventToEvent = unsafeToEvent;
var rtcPeerConnectionIceEventToEvent = unsafeToEvent;
var rtcIdentityEventToEvent = unsafeToEvent;
var rtcIdentityErrorEventToEvent = unsafeToEvent;
var rtcDataChannelEventToEvent = unsafeToEvent;
var relatedEventToEvent = unsafeToEvent;
var readWheelEvent = Data_Foreign.unsafeReadTagged("WheelEvent");
var readUserProximityEvent = Data_Foreign.unsafeReadTagged("UserProximityEvent");
var readUIEvent = Data_Foreign.unsafeReadTagged("UIEvent");
var readTransitionEvent = Data_Foreign.unsafeReadTagged("TransitionEvent");
var readTouchEvent = Data_Foreign.unsafeReadTagged("TouchEvent");
var readTimeEvent = Data_Foreign.unsafeReadTagged("TimeEvent");
var readStorageEvent = Data_Foreign.unsafeReadTagged("StorageEvent");
var readSensorEvent = Data_Foreign.unsafeReadTagged("SensorEvent");
var readSVGZoomEvent = Data_Foreign.unsafeReadTagged("SVGZoomEvent");
var readSVGEvent = Data_Foreign.unsafeReadTagged("SVGEvent");
var readRelatedEvent = Data_Foreign.unsafeReadTagged("RelatedEvent");
var readRTCPeerConnectionIceEvent = Data_Foreign.unsafeReadTagged("RTCPeerConnectionIceEvent");
var readRTCIdentityEvent = Data_Foreign.unsafeReadTagged("RTCIdentityEvent");
var readRTCIdentityErrorEvent = Data_Foreign.unsafeReadTagged("RTCIdentityErrorEvent");
var readRTCDataChannelEvent = Data_Foreign.unsafeReadTagged("RTCDataChannelEvent");
var readProgressEvent = Data_Foreign.unsafeReadTagged("ProgressEvent");
var readPopStateEvent = Data_Foreign.unsafeReadTagged("PopStateEvent");
var readPointerEvent = Data_Foreign.unsafeReadTagged("PointerEvent");
var readPageTransitionEvent = Data_Foreign.unsafeReadTagged("PageTransitionEvent");
var readOfflineAudioCompletionEvent = Data_Foreign.unsafeReadTagged("OfflineAudioCompletionEvent");
var readMutationEvent = Data_Foreign.unsafeReadTagged("MutationEvent");
var readMouseEvent = Data_Foreign.unsafeReadTagged("MouseEvent");
var readMessageEvent = Data_Foreign.unsafeReadTagged("MessageEvent");
var readMediaStreamEvent = Data_Foreign.unsafeReadTagged("MediaStreamEvent");
var readKeyboardEvent = Data_Foreign.unsafeReadTagged("KeyboardEvent");
var readInputEvent = Data_Foreign.unsafeReadTagged("InputEvent");
var readIDBVersionChangeEvent = Data_Foreign.unsafeReadTagged("IDBVersionChangeEvent");
var readHashChangeEvent = Data_Foreign.unsafeReadTagged("HashChangeEvent");
var readGamepadEvent = Data_Foreign.unsafeReadTagged("GamepadEvent");
var readFocusEvent = Data_Foreign.unsafeReadTagged("FocusEvent");
var readFetchEvent = Data_Foreign.unsafeReadTagged("FetchEvent");
var readEventTarget = $foreign._readEventTarget(Data_Either.Left.create)(Data_Either.Right.create);
var readErrorEvent = Data_Foreign.unsafeReadTagged("ErrorEvent");
var readEditingBeforeInputEvent = Data_Foreign.unsafeReadTagged("EditingBeforeInputEvent");
var readDragEvent = Data_Foreign.unsafeReadTagged("DragEvent");
var readDeviceProximityEvent = Data_Foreign.unsafeReadTagged("DeviceProximityEvent");
var readDeviceOrientationEvent = Data_Foreign.unsafeReadTagged("DeviceOrientationEvent");
var readDeviceMotionEvent = Data_Foreign.unsafeReadTagged("DeviceMotionEvent");
var readDeviceLightEvent = Data_Foreign.unsafeReadTagged("DeviceLightEvent");
var readDOMTransactionEvent = Data_Foreign.unsafeReadTagged("DOMTransactionEvent");
var readCustomEvent = Data_Foreign.unsafeReadTagged("CustomEvent");
var readCompositionEvent = Data_Foreign.unsafeReadTagged("CompositionEvent");
var readCloseEvent = Data_Foreign.unsafeReadTagged("CloseEvent");
var readClipboardEvent = Data_Foreign.unsafeReadTagged("ClipboardEvent");
var readCSSFontFaceLoadEvent = Data_Foreign.unsafeReadTagged("CSSFontFaceLoadEvent");
var readBlobEvent = Data_Foreign.unsafeReadTagged("BlobEvent");
var readBeforeUnloadEvent = Data_Foreign.unsafeReadTagged("BeforeUnloadEvent");
var readBeforeInputEvent = Data_Foreign.unsafeReadTagged("BeforeInputEvent");
var readAudioProcessingEvent = Data_Foreign.unsafeReadTagged("AudioProcessingEvent");
var readAnimationEvent = Data_Foreign.unsafeReadTagged("AnimationEvent");
var progressEventToEvent = unsafeToEvent;
var popStateEventToEvent = unsafeToEvent;
var pointerEventToEvent = unsafeToEvent;
var pageTransitionEventToEvent = unsafeToEvent;
var offlineAudioCompletionEventToEvent = unsafeToEvent;
var mutationEventToEvent = unsafeToEvent;
var mouseEventToEvent = unsafeToEvent;
var messageEventToEvent = unsafeToEvent;
var mediaStreamEventToEvent = unsafeToEvent;
var keyboardEventToEvent = unsafeToEvent;
var isForeignWheelEvent = new Data_Foreign_Class.IsForeign(readWheelEvent);
var isForeignUserProximityEvent = new Data_Foreign_Class.IsForeign(readUserProximityEvent);
var isForeignUIEvent = new Data_Foreign_Class.IsForeign(readUIEvent);
var isForeignTransitionEvent = new Data_Foreign_Class.IsForeign(readTransitionEvent);
var isForeignTouchEvent = new Data_Foreign_Class.IsForeign(readTouchEvent);
var isForeignTimeEvent = new Data_Foreign_Class.IsForeign(readTimeEvent);
var isForeignStorageEvent = new Data_Foreign_Class.IsForeign(readStorageEvent);
var isForeignSensorEvent = new Data_Foreign_Class.IsForeign(readSensorEvent);
var isForeignSVGZoomEvent = new Data_Foreign_Class.IsForeign(readSVGZoomEvent);
var isForeignSVGEvent = new Data_Foreign_Class.IsForeign(readSVGEvent);
var isForeignRelatedEvent = new Data_Foreign_Class.IsForeign(readRelatedEvent);
var isForeignRTCPeerConnectionIceEvent = new Data_Foreign_Class.IsForeign(readRTCPeerConnectionIceEvent);
var isForeignRTCIdentityEvent = new Data_Foreign_Class.IsForeign(readRTCIdentityEvent);
var isForeignRTCIdentityErrorEvent = new Data_Foreign_Class.IsForeign(readRTCIdentityErrorEvent);
var isForeignRTCDataChannelEvent = new Data_Foreign_Class.IsForeign(readRTCDataChannelEvent);
var isForeignProgressEvent = new Data_Foreign_Class.IsForeign(readProgressEvent);
var isForeignPopStateEvent = new Data_Foreign_Class.IsForeign(readPopStateEvent);
var isForeignPointerEvent = new Data_Foreign_Class.IsForeign(readPointerEvent);
var isForeignPageTransitionEvent = new Data_Foreign_Class.IsForeign(readPageTransitionEvent);
var isForeignOfflineAudioCompletionEvent = new Data_Foreign_Class.IsForeign(readOfflineAudioCompletionEvent);
var isForeignMutationEvent = new Data_Foreign_Class.IsForeign(readMutationEvent);
var isForeignMouseEvent = new Data_Foreign_Class.IsForeign(readMouseEvent);
var isForeignMessageEvent = new Data_Foreign_Class.IsForeign(readMessageEvent);
var isForeignMediaStreamEvent = new Data_Foreign_Class.IsForeign(readMediaStreamEvent);
var isForeignKeyboardEvent = new Data_Foreign_Class.IsForeign(readKeyboardEvent);
var isForeignInputEvent = new Data_Foreign_Class.IsForeign(readInputEvent);
var isForeignIDBVersionChangeEvent = new Data_Foreign_Class.IsForeign(readIDBVersionChangeEvent);
var isForeignHashChangeEvent = new Data_Foreign_Class.IsForeign(readHashChangeEvent);
var isForeignGamepadEvent = new Data_Foreign_Class.IsForeign(readGamepadEvent);
var isForeignFocusEvent = new Data_Foreign_Class.IsForeign(readFocusEvent);
var isForeignFetchEvent = new Data_Foreign_Class.IsForeign(readFetchEvent);
var isForeignEventTarget = new Data_Foreign_Class.IsForeign(readEventTarget);
var isForeignErrorEvent = new Data_Foreign_Class.IsForeign(readErrorEvent);
var isForeignEditingBeforeInputEvent = new Data_Foreign_Class.IsForeign(readEditingBeforeInputEvent);
var isForeignDragEvent = new Data_Foreign_Class.IsForeign(readDragEvent);
var isForeignDeviceProximityEvent = new Data_Foreign_Class.IsForeign(readDeviceProximityEvent);
var isForeignDeviceOrientationEvent = new Data_Foreign_Class.IsForeign(readDeviceOrientationEvent);
var isForeignDeviceMotionEvent = new Data_Foreign_Class.IsForeign(readDeviceMotionEvent);
var isForeignDeviceLightEvent = new Data_Foreign_Class.IsForeign(readDeviceLightEvent);
var isForeignDOMTransactionEvent = new Data_Foreign_Class.IsForeign(readDOMTransactionEvent);
var isForeignCustomEvent = new Data_Foreign_Class.IsForeign(readCustomEvent);
var isForeignCompositionEvent = new Data_Foreign_Class.IsForeign(readCompositionEvent);
var isForeignCloseEvent = new Data_Foreign_Class.IsForeign(readCloseEvent);
var isForeignClipboardEvent = new Data_Foreign_Class.IsForeign(readClipboardEvent);
var isForeignCSSFontFaceLoadEvent = new Data_Foreign_Class.IsForeign(readCSSFontFaceLoadEvent);
var isForeignBlobEvent = new Data_Foreign_Class.IsForeign(readBlobEvent);
var isForeignBeforeUnloadEvent = new Data_Foreign_Class.IsForeign(readBeforeUnloadEvent);
var isForeignBeforeInputEvent = new Data_Foreign_Class.IsForeign(readBeforeInputEvent);
var isForeignAudioProcessingEvent = new Data_Foreign_Class.IsForeign(readAudioProcessingEvent);
var isForeignAnimationEvent = new Data_Foreign_Class.IsForeign(readAnimationEvent);
var inputEventToEvent = unsafeToEvent;
var idbVersionChangeEventToEvent = unsafeToEvent;
var hashChangeEventToEvent = unsafeToEvent;
var gamepadEventToEvent = unsafeToEvent;
var focusEventToEvent = unsafeToEvent;
var fetchEventToEvent = unsafeToEvent;
var errorEventToEvent = unsafeToEvent;
var eqEventType = new Prelude.Eq(function (v) {
    return function (v1) {
        return v === v1;
    };
});
var ordEventType = new Prelude.Ord(function () {
    return eqEventType;
}, function (v) {
    return function (v1) {
        return Prelude.compare(Prelude.ordString)(v)(v1);
    };
});
var editingBeforeInputEventToEvent = unsafeToEvent;
var dragEventToEvent = unsafeToEvent;
var domTransactionEventToEvent = unsafeToEvent;
var deviceProximityEventToEvent = unsafeToEvent;
var deviceOrientationEventToEvent = unsafeToEvent;
var deviceMotionEventToEvent = unsafeToEvent;
var deviceLightEventToEvent = unsafeToEvent;
var customEventToEvent = unsafeToEvent;
var cssFontFaceLoadEventToEvent = unsafeToEvent;
var compositionEventToEvent = unsafeToEvent;
var closeEventToEvent = unsafeToEvent;
var clipboardEventToEvent = unsafeToEvent;
var blobEventToEvent = unsafeToEvent;
var beforeUnloadEventToEvent = unsafeToEvent;
var beforeInputEventToEvent = unsafeToEvent;
var audioProcessingEventToEvent = unsafeToEvent;
var animationEventToEvent = unsafeToEvent;
module.exports = {
    EventType: EventType, 
    readWheelEvent: readWheelEvent, 
    wheelEventToEvent: wheelEventToEvent, 
    readUserProximityEvent: readUserProximityEvent, 
    userProximityEventToEvent: userProximityEventToEvent, 
    readUIEvent: readUIEvent, 
    uiEventToEvent: uiEventToEvent, 
    readTransitionEvent: readTransitionEvent, 
    transitionEventToEvent: transitionEventToEvent, 
    readTouchEvent: readTouchEvent, 
    touchEventToEvent: touchEventToEvent, 
    readTimeEvent: readTimeEvent, 
    timeEventToEvent: timeEventToEvent, 
    readSVGZoomEvent: readSVGZoomEvent, 
    svgZoomEventToEvent: svgZoomEventToEvent, 
    readSVGEvent: readSVGEvent, 
    svgEventToEvent: svgEventToEvent, 
    readStorageEvent: readStorageEvent, 
    storageEventToEvent: storageEventToEvent, 
    readSensorEvent: readSensorEvent, 
    sensorEventToEvent: sensorEventToEvent, 
    readRTCPeerConnectionIceEvent: readRTCPeerConnectionIceEvent, 
    rtcPeerConnectionIceEventToEvent: rtcPeerConnectionIceEventToEvent, 
    readRTCIdentityEvent: readRTCIdentityEvent, 
    rtcIdentityEventToEvent: rtcIdentityEventToEvent, 
    readRTCIdentityErrorEvent: readRTCIdentityErrorEvent, 
    rtcIdentityErrorEventToEvent: rtcIdentityErrorEventToEvent, 
    readRTCDataChannelEvent: readRTCDataChannelEvent, 
    rtcDataChannelEventToEvent: rtcDataChannelEventToEvent, 
    readRelatedEvent: readRelatedEvent, 
    relatedEventToEvent: relatedEventToEvent, 
    readProgressEvent: readProgressEvent, 
    progressEventToEvent: progressEventToEvent, 
    readPopStateEvent: readPopStateEvent, 
    popStateEventToEvent: popStateEventToEvent, 
    readPointerEvent: readPointerEvent, 
    pointerEventToEvent: pointerEventToEvent, 
    readPageTransitionEvent: readPageTransitionEvent, 
    pageTransitionEventToEvent: pageTransitionEventToEvent, 
    readOfflineAudioCompletionEvent: readOfflineAudioCompletionEvent, 
    offlineAudioCompletionEventToEvent: offlineAudioCompletionEventToEvent, 
    readMutationEvent: readMutationEvent, 
    mutationEventToEvent: mutationEventToEvent, 
    readMouseEvent: readMouseEvent, 
    mouseEventToEvent: mouseEventToEvent, 
    readMessageEvent: readMessageEvent, 
    messageEventToEvent: messageEventToEvent, 
    readMediaStreamEvent: readMediaStreamEvent, 
    mediaStreamEventToEvent: mediaStreamEventToEvent, 
    readKeyboardEvent: readKeyboardEvent, 
    keyboardEventToEvent: keyboardEventToEvent, 
    readInputEvent: readInputEvent, 
    inputEventToEvent: inputEventToEvent, 
    readIDBVersionChangeEvent: readIDBVersionChangeEvent, 
    idbVersionChangeEventToEvent: idbVersionChangeEventToEvent, 
    readHashChangeEvent: readHashChangeEvent, 
    hashChangeEventToEvent: hashChangeEventToEvent, 
    readGamepadEvent: readGamepadEvent, 
    gamepadEventToEvent: gamepadEventToEvent, 
    readFocusEvent: readFocusEvent, 
    focusEventToEvent: focusEventToEvent, 
    readFetchEvent: readFetchEvent, 
    fetchEventToEvent: fetchEventToEvent, 
    readErrorEvent: readErrorEvent, 
    errorEventToEvent: errorEventToEvent, 
    readEditingBeforeInputEvent: readEditingBeforeInputEvent, 
    editingBeforeInputEventToEvent: editingBeforeInputEventToEvent, 
    readDragEvent: readDragEvent, 
    dragEventToEvent: dragEventToEvent, 
    readDOMTransactionEvent: readDOMTransactionEvent, 
    domTransactionEventToEvent: domTransactionEventToEvent, 
    readDeviceProximityEvent: readDeviceProximityEvent, 
    deviceProximityEventToEvent: deviceProximityEventToEvent, 
    readDeviceOrientationEvent: readDeviceOrientationEvent, 
    deviceOrientationEventToEvent: deviceOrientationEventToEvent, 
    readDeviceMotionEvent: readDeviceMotionEvent, 
    deviceMotionEventToEvent: deviceMotionEventToEvent, 
    readDeviceLightEvent: readDeviceLightEvent, 
    deviceLightEventToEvent: deviceLightEventToEvent, 
    readCustomEvent: readCustomEvent, 
    customEventToEvent: customEventToEvent, 
    readCSSFontFaceLoadEvent: readCSSFontFaceLoadEvent, 
    cssFontFaceLoadEventToEvent: cssFontFaceLoadEventToEvent, 
    readCompositionEvent: readCompositionEvent, 
    compositionEventToEvent: compositionEventToEvent, 
    readCloseEvent: readCloseEvent, 
    closeEventToEvent: closeEventToEvent, 
    readClipboardEvent: readClipboardEvent, 
    clipboardEventToEvent: clipboardEventToEvent, 
    readBlobEvent: readBlobEvent, 
    blobEventToEvent: blobEventToEvent, 
    readBeforeUnloadEvent: readBeforeUnloadEvent, 
    beforeUnloadEventToEvent: beforeUnloadEventToEvent, 
    readBeforeInputEvent: readBeforeInputEvent, 
    beforeInputEventToEvent: beforeInputEventToEvent, 
    readAudioProcessingEvent: readAudioProcessingEvent, 
    audioProcessingEventToEvent: audioProcessingEventToEvent, 
    readAnimationEvent: readAnimationEvent, 
    animationEventToEvent: animationEventToEvent, 
    readEventTarget: readEventTarget, 
    eqEventType: eqEventType, 
    ordEventType: ordEventType, 
    isForeignEventTarget: isForeignEventTarget, 
    isForeignAnimationEvent: isForeignAnimationEvent, 
    isForeignAudioProcessingEvent: isForeignAudioProcessingEvent, 
    isForeignBeforeInputEvent: isForeignBeforeInputEvent, 
    isForeignBeforeUnloadEvent: isForeignBeforeUnloadEvent, 
    isForeignBlobEvent: isForeignBlobEvent, 
    isForeignClipboardEvent: isForeignClipboardEvent, 
    isForeignCloseEvent: isForeignCloseEvent, 
    isForeignCompositionEvent: isForeignCompositionEvent, 
    isForeignCSSFontFaceLoadEvent: isForeignCSSFontFaceLoadEvent, 
    isForeignCustomEvent: isForeignCustomEvent, 
    isForeignDeviceLightEvent: isForeignDeviceLightEvent, 
    isForeignDeviceMotionEvent: isForeignDeviceMotionEvent, 
    isForeignDeviceOrientationEvent: isForeignDeviceOrientationEvent, 
    isForeignDeviceProximityEvent: isForeignDeviceProximityEvent, 
    isForeignDOMTransactionEvent: isForeignDOMTransactionEvent, 
    isForeignDragEvent: isForeignDragEvent, 
    isForeignEditingBeforeInputEvent: isForeignEditingBeforeInputEvent, 
    isForeignErrorEvent: isForeignErrorEvent, 
    isForeignFetchEvent: isForeignFetchEvent, 
    isForeignFocusEvent: isForeignFocusEvent, 
    isForeignGamepadEvent: isForeignGamepadEvent, 
    isForeignHashChangeEvent: isForeignHashChangeEvent, 
    isForeignIDBVersionChangeEvent: isForeignIDBVersionChangeEvent, 
    isForeignInputEvent: isForeignInputEvent, 
    isForeignKeyboardEvent: isForeignKeyboardEvent, 
    isForeignMediaStreamEvent: isForeignMediaStreamEvent, 
    isForeignMessageEvent: isForeignMessageEvent, 
    isForeignMouseEvent: isForeignMouseEvent, 
    isForeignMutationEvent: isForeignMutationEvent, 
    isForeignOfflineAudioCompletionEvent: isForeignOfflineAudioCompletionEvent, 
    isForeignPageTransitionEvent: isForeignPageTransitionEvent, 
    isForeignPointerEvent: isForeignPointerEvent, 
    isForeignPopStateEvent: isForeignPopStateEvent, 
    isForeignProgressEvent: isForeignProgressEvent, 
    isForeignRelatedEvent: isForeignRelatedEvent, 
    isForeignRTCDataChannelEvent: isForeignRTCDataChannelEvent, 
    isForeignRTCIdentityErrorEvent: isForeignRTCIdentityErrorEvent, 
    isForeignRTCIdentityEvent: isForeignRTCIdentityEvent, 
    isForeignRTCPeerConnectionIceEvent: isForeignRTCPeerConnectionIceEvent, 
    isForeignSensorEvent: isForeignSensorEvent, 
    isForeignStorageEvent: isForeignStorageEvent, 
    isForeignSVGEvent: isForeignSVGEvent, 
    isForeignSVGZoomEvent: isForeignSVGZoomEvent, 
    isForeignTimeEvent: isForeignTimeEvent, 
    isForeignTouchEvent: isForeignTouchEvent, 
    isForeignTransitionEvent: isForeignTransitionEvent, 
    isForeignUIEvent: isForeignUIEvent, 
    isForeignUserProximityEvent: isForeignUserProximityEvent, 
    isForeignWheelEvent: isForeignWheelEvent
};

},{"../Data.Either":114,"../Data.Foreign":128,"../Data.Foreign.Class":121,"../Prelude":203,"../Unsafe.Coerce":206,"./foreign":83}],85:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Unsafe_Coerce = require("../Unsafe.Coerce");
var fileToBlob = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    fileToBlob: fileToBlob
};

},{"../Unsafe.Coerce":206}],86:[function(require,module,exports){
"use strict";

// module DOM.HTML.Types

exports._readHTMLElement = function (failure) {
  return function (success) {
    return function (value) {
      var tag = Object.prototype.toString.call(value);
      if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
        return success(value);
      } else {
        return failure(tag);
      }
    };
  };
};

},{}],87:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Foreign_Class = require("../Data.Foreign.Class");
var DOM_Event_Types = require("../DOM.Event.Types");
var DOM_Node_Types = require("../DOM.Node.Types");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var windowToEventTarget = Unsafe_Coerce.unsafeCoerce;
var readHTMLVideoElement = Data_Foreign.unsafeReadTagged("HTMLVideoElement");
var readHTMLUListElement = Data_Foreign.unsafeReadTagged("HTMLUListElement");
var readHTMLTrackElement = Data_Foreign.unsafeReadTagged("HTMLTrackElement");
var readHTMLTitleElement = Data_Foreign.unsafeReadTagged("HTMLTitleElement");
var readHTMLTimeElement = Data_Foreign.unsafeReadTagged("HTMLTimeElement");
var readHTMLTextAreaElement = Data_Foreign.unsafeReadTagged("HTMLTextAreaElement");
var readHTMLTemplateElement = Data_Foreign.unsafeReadTagged("HTMLTemplateElement");
var readHTMLTableSectionElement = Data_Foreign.unsafeReadTagged("HTMLTableSectionElement");
var readHTMLTableRowElement = Data_Foreign.unsafeReadTagged("HTMLTableRowElement");
var readHTMLTableHeaderCellElement = Data_Foreign.unsafeReadTagged("HTMLTableHeaderCellElement");
var readHTMLTableElement = Data_Foreign.unsafeReadTagged("HTMLTableElement");
var readHTMLTableDataCellElement = Data_Foreign.unsafeReadTagged("HTMLTableDataCellElement");
var readHTMLTableColElement = Data_Foreign.unsafeReadTagged("HTMLTableColElement");
var readHTMLTableCellElement = Data_Foreign.unsafeReadTagged("HTMLTableCellElement");
var readHTMLTableCaptionElement = Data_Foreign.unsafeReadTagged("HTMLTableCaptionElement");
var readHTMLStyleElement = Data_Foreign.unsafeReadTagged("HTMLStyleElement");
var readHTMLSpanElement = Data_Foreign.unsafeReadTagged("HTMLSpanElement");
var readHTMLSourceElement = Data_Foreign.unsafeReadTagged("HTMLSourceElement");
var readHTMLSelectElement = Data_Foreign.unsafeReadTagged("HTMLSelectElement");
var readHTMLScriptElement = Data_Foreign.unsafeReadTagged("HTMLScriptElement");
var readHTMLQuoteElement = Data_Foreign.unsafeReadTagged("HTMLQuoteElement");
var readHTMLProgressElement = Data_Foreign.unsafeReadTagged("HTMLProgressElement");
var readHTMLPreElement = Data_Foreign.unsafeReadTagged("HTMLPreElement");
var readHTMLParamElement = Data_Foreign.unsafeReadTagged("HTMLParamElement");
var readHTMLParagraphElement = Data_Foreign.unsafeReadTagged("HTMLParagraphElement");
var readHTMLOutputElement = Data_Foreign.unsafeReadTagged("HTMLOutputElement");
var readHTMLOptionElement = Data_Foreign.unsafeReadTagged("HTMLOptionElement");
var readHTMLOptGroupElement = Data_Foreign.unsafeReadTagged("HTMLOptGroupElement");
var readHTMLObjectElement = Data_Foreign.unsafeReadTagged("HTMLObjectElement");
var readHTMLOListElement = Data_Foreign.unsafeReadTagged("HTMLOListElement");
var readHTMLModElement = Data_Foreign.unsafeReadTagged("HTMLModElement");
var readHTMLMeterElement = Data_Foreign.unsafeReadTagged("HTMLMeterElement");
var readHTMLMetaElement = Data_Foreign.unsafeReadTagged("HTMLMetaElement");
var readHTMLMediaElement = Data_Foreign.unsafeReadTagged("HTMLMediaElement");
var readHTMLMapElement = Data_Foreign.unsafeReadTagged("HTMLMapElement");
var readHTMLLinkElement = Data_Foreign.unsafeReadTagged("HTMLLinkElement");
var readHTMLLegendElement = Data_Foreign.unsafeReadTagged("HTMLLegendElement");
var readHTMLLabelElement = Data_Foreign.unsafeReadTagged("HTMLLabelElement");
var readHTMLLIElement = Data_Foreign.unsafeReadTagged("HTMLLIElement");
var readHTMLKeygenElement = Data_Foreign.unsafeReadTagged("HTMLKeygenElement");
var readHTMLInputElement = Data_Foreign.unsafeReadTagged("HTMLInputElement");
var readHTMLImageElement = Data_Foreign.unsafeReadTagged("HTMLImageElement");
var readHTMLIFrameElement = Data_Foreign.unsafeReadTagged("HTMLIFrameElement");
var readHTMLHtmlElement = Data_Foreign.unsafeReadTagged("HTMLHtmlElement");
var readHTMLHeadingElement = Data_Foreign.unsafeReadTagged("HTMLHeadingElement");
var readHTMLHeadElement = Data_Foreign.unsafeReadTagged("HTMLHeadElement");
var readHTMLHRElement = Data_Foreign.unsafeReadTagged("HTMLHRElement");
var readHTMLFormElement = Data_Foreign.unsafeReadTagged("HTMLFormElement");
var readHTMLFieldSetElement = Data_Foreign.unsafeReadTagged("HTMLFieldSetElement");
var readHTMLEmbedElement = Data_Foreign.unsafeReadTagged("HTMLEmbedElement");
var readHTMLElement = $foreign._readHTMLElement(function ($0) {
    return Data_Either.Left.create(Data_Foreign.TypeMismatch.create("HTMLElement")($0));
})(Data_Either.Right.create);
var readHTMLDocument = Data_Foreign.unsafeReadTagged("HTMLDocument");
var readHTMLDivElement = Data_Foreign.unsafeReadTagged("HTMLDivElement");
var readHTMLDataListElement = Data_Foreign.unsafeReadTagged("HTMLDataListElement");
var readHTMLDataElement = Data_Foreign.unsafeReadTagged("HTMLDataElement");
var readHTMLDListElement = Data_Foreign.unsafeReadTagged("HTMLDListElement");
var readHTMLCanvasElement = Data_Foreign.unsafeReadTagged("HTMLCanvasElement");
var readHTMLButtonElement = Data_Foreign.unsafeReadTagged("HTMLButtonElement");
var readHTMLBodyElement = Data_Foreign.unsafeReadTagged("HTMLBodyElement");
var readHTMLBaseElement = Data_Foreign.unsafeReadTagged("HTMLBaseElement");
var readHTMLBRElement = Data_Foreign.unsafeReadTagged("HTMLBRElement");
var readHTMLAudioElement = Data_Foreign.unsafeReadTagged("HTMLAudioElement");
var readHTMLAreaElement = Data_Foreign.unsafeReadTagged("HTMLAreaElement");
var readHTMLAnchorElement = Data_Foreign.unsafeReadTagged("HTMLAnchorElement");
var isForeignHTMLUListElement = new Data_Foreign_Class.IsForeign(readHTMLUListElement);
var isForeignHTMLTrackElement = new Data_Foreign_Class.IsForeign(readHTMLTrackElement);
var isForeignHTMLTitleElement = new Data_Foreign_Class.IsForeign(readHTMLTitleElement);
var isForeignHTMLTimeElement = new Data_Foreign_Class.IsForeign(readHTMLTimeElement);
var isForeignHTMLTextAreaElement = new Data_Foreign_Class.IsForeign(readHTMLTextAreaElement);
var isForeignHTMLTemplateElement = new Data_Foreign_Class.IsForeign(readHTMLTemplateElement);
var isForeignHTMLTableSectionElement = new Data_Foreign_Class.IsForeign(readHTMLTableSectionElement);
var isForeignHTMLTableRowElement = new Data_Foreign_Class.IsForeign(readHTMLTableRowElement);
var isForeignHTMLTableElement = new Data_Foreign_Class.IsForeign(readHTMLTableElement);
var isForeignHTMLTableColElement = new Data_Foreign_Class.IsForeign(readHTMLTableColElement);
var isForeignHTMLTableCaptionElement = new Data_Foreign_Class.IsForeign(readHTMLTableCaptionElement);
var isForeignHTMLStyleElement = new Data_Foreign_Class.IsForeign(readHTMLStyleElement);
var isForeignHTMLSpanElement = new Data_Foreign_Class.IsForeign(readHTMLSpanElement);
var isForeignHTMLSourceElement = new Data_Foreign_Class.IsForeign(readHTMLSourceElement);
var isForeignHTMLSelectElement = new Data_Foreign_Class.IsForeign(readHTMLSelectElement);
var isForeignHTMLScriptElement = new Data_Foreign_Class.IsForeign(readHTMLScriptElement);
var isForeignHTMLQuoteElement = new Data_Foreign_Class.IsForeign(readHTMLQuoteElement);
var isForeignHTMLProgressElement = new Data_Foreign_Class.IsForeign(readHTMLProgressElement);
var isForeignHTMLPreElement = new Data_Foreign_Class.IsForeign(readHTMLPreElement);
var isForeignHTMLParamElement = new Data_Foreign_Class.IsForeign(readHTMLParamElement);
var isForeignHTMLParagraphElement = new Data_Foreign_Class.IsForeign(readHTMLParagraphElement);
var isForeignHTMLOutputElement = new Data_Foreign_Class.IsForeign(readHTMLOutputElement);
var isForeignHTMLOptionElement = new Data_Foreign_Class.IsForeign(readHTMLOptionElement);
var isForeignHTMLOptGroupElement = new Data_Foreign_Class.IsForeign(readHTMLOptGroupElement);
var isForeignHTMLObjectElement = new Data_Foreign_Class.IsForeign(readHTMLObjectElement);
var isForeignHTMLOListElement = new Data_Foreign_Class.IsForeign(readHTMLOListElement);
var isForeignHTMLModElement = new Data_Foreign_Class.IsForeign(readHTMLModElement);
var isForeignHTMLMeterElement = new Data_Foreign_Class.IsForeign(readHTMLMeterElement);
var isForeignHTMLMetaElement = new Data_Foreign_Class.IsForeign(readHTMLMetaElement);
var isForeignHTMLMapElement = new Data_Foreign_Class.IsForeign(readHTMLMapElement);
var isForeignHTMLLinkElement = new Data_Foreign_Class.IsForeign(readHTMLLinkElement);
var isForeignHTMLLegendElement = new Data_Foreign_Class.IsForeign(readHTMLLegendElement);
var isForeignHTMLLabelElement = new Data_Foreign_Class.IsForeign(readHTMLLabelElement);
var isForeignHTMLLIElement = new Data_Foreign_Class.IsForeign(readHTMLLIElement);
var isForeignHTMLKeygenElement = new Data_Foreign_Class.IsForeign(readHTMLKeygenElement);
var isForeignHTMLInputElement = new Data_Foreign_Class.IsForeign(readHTMLInputElement);
var isForeignHTMLImageElement = new Data_Foreign_Class.IsForeign(readHTMLImageElement);
var isForeignHTMLIFrameElement = new Data_Foreign_Class.IsForeign(readHTMLIFrameElement);
var isForeignHTMLHtmlElement = new Data_Foreign_Class.IsForeign(readHTMLHtmlElement);
var isForeignHTMLHeadingElement = new Data_Foreign_Class.IsForeign(readHTMLHeadingElement);
var isForeignHTMLHeadElement = new Data_Foreign_Class.IsForeign(readHTMLHeadElement);
var isForeignHTMLHRElement = new Data_Foreign_Class.IsForeign(readHTMLHRElement);
var isForeignHTMLFormElement = new Data_Foreign_Class.IsForeign(readHTMLFormElement);
var isForeignHTMLFieldSetElement = new Data_Foreign_Class.IsForeign(readHTMLFieldSetElement);
var isForeignHTMLEmbedElement = new Data_Foreign_Class.IsForeign(readHTMLEmbedElement);
var isForeignHTMLElement = new Data_Foreign_Class.IsForeign(readHTMLElement);
var isForeignHTMLDocument = new Data_Foreign_Class.IsForeign(readHTMLDocument);
var isForeignHTMLDivElement = new Data_Foreign_Class.IsForeign(readHTMLDivElement);
var isForeignHTMLDataListElement = new Data_Foreign_Class.IsForeign(readHTMLDataListElement);
var isForeignHTMLDataElement = new Data_Foreign_Class.IsForeign(readHTMLDataElement);
var isForeignHTMLDListElement = new Data_Foreign_Class.IsForeign(readHTMLDListElement);
var isForeignHTMLCanvasElement = new Data_Foreign_Class.IsForeign(readHTMLCanvasElement);
var isForeignHTMLButtonElement = new Data_Foreign_Class.IsForeign(readHTMLButtonElement);
var isForeignHTMLBodyElement = new Data_Foreign_Class.IsForeign(readHTMLBodyElement);
var isForeignHTMLBaseElement = new Data_Foreign_Class.IsForeign(readHTMLBaseElement);
var isForeignHTMLBRElement = new Data_Foreign_Class.IsForeign(readHTMLBRElement);
var isForeignHTMLAreaElement = new Data_Foreign_Class.IsForeign(readHTMLAreaElement);
var isForeignHTMLAnchorElement = new Data_Foreign_Class.IsForeign(readHTMLAnchorElement);
var htmlVideoElementToHTMLMediaElement = Unsafe_Coerce.unsafeCoerce;
var htmlUListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTrackElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTitleElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTimeElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTextAreaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTemplateElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableSectionElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableRowElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableHeaderCellElementToHTMLTableCellElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableDataCellElementToHTMLTableCellElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableColElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableCellElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableCaptionElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlStyleElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlSpanElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlSourceElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlSelectElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlScriptElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlQuoteElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlProgressElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlPreElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlParamElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlParagraphElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOutputElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOptionElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOptGroupElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlObjectElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlModElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMeterElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMetaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMediaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMapElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLinkElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLegendElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLabelElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLIElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlKeygenElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlInputElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlImageElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlIFrameElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHtmlElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHeadingElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHeadElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHRElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlFormElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlFieldSetElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlEmbedElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlElementToParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToEventTarget = Unsafe_Coerce.unsafeCoerce;
var htmlElementToElement = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToEventTarget = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToDocument = Unsafe_Coerce.unsafeCoerce;
var htmlDivElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlDataListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlDataElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlDListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlCanvasElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlButtonElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlBodyElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlBaseElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlBRElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlAudioElementToHTMLMediaElement = Unsafe_Coerce.unsafeCoerce;
var htmlAreaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlAnchorElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    readHTMLCanvasElement: readHTMLCanvasElement, 
    htmlCanvasElementToHTMLElement: htmlCanvasElementToHTMLElement, 
    readHTMLTemplateElement: readHTMLTemplateElement, 
    htmlTemplateElementToHTMLElement: htmlTemplateElementToHTMLElement, 
    readHTMLScriptElement: readHTMLScriptElement, 
    htmlScriptElementToHTMLElement: htmlScriptElementToHTMLElement, 
    readHTMLLegendElement: readHTMLLegendElement, 
    htmlLegendElementToHTMLElement: htmlLegendElementToHTMLElement, 
    readHTMLFieldSetElement: readHTMLFieldSetElement, 
    htmlFieldSetElementToHTMLElement: htmlFieldSetElementToHTMLElement, 
    readHTMLMeterElement: readHTMLMeterElement, 
    htmlMeterElementToHTMLElement: htmlMeterElementToHTMLElement, 
    readHTMLProgressElement: readHTMLProgressElement, 
    htmlProgressElementToHTMLElement: htmlProgressElementToHTMLElement, 
    readHTMLOutputElement: readHTMLOutputElement, 
    htmlOutputElementToHTMLElement: htmlOutputElementToHTMLElement, 
    readHTMLKeygenElement: readHTMLKeygenElement, 
    htmlKeygenElementToHTMLElement: htmlKeygenElementToHTMLElement, 
    readHTMLTextAreaElement: readHTMLTextAreaElement, 
    htmlTextAreaElementToHTMLElement: htmlTextAreaElementToHTMLElement, 
    readHTMLOptionElement: readHTMLOptionElement, 
    htmlOptionElementToHTMLElement: htmlOptionElementToHTMLElement, 
    readHTMLOptGroupElement: readHTMLOptGroupElement, 
    htmlOptGroupElementToHTMLElement: htmlOptGroupElementToHTMLElement, 
    readHTMLDataListElement: readHTMLDataListElement, 
    htmlDataListElementToHTMLElement: htmlDataListElementToHTMLElement, 
    readHTMLSelectElement: readHTMLSelectElement, 
    htmlSelectElementToHTMLElement: htmlSelectElementToHTMLElement, 
    readHTMLButtonElement: readHTMLButtonElement, 
    htmlButtonElementToHTMLElement: htmlButtonElementToHTMLElement, 
    readHTMLInputElement: readHTMLInputElement, 
    htmlInputElementToHTMLElement: htmlInputElementToHTMLElement, 
    readHTMLLabelElement: readHTMLLabelElement, 
    htmlLabelElementToHTMLElement: htmlLabelElementToHTMLElement, 
    readHTMLFormElement: readHTMLFormElement, 
    htmlFormElementToHTMLElement: htmlFormElementToHTMLElement, 
    readHTMLTableHeaderCellElement: readHTMLTableHeaderCellElement, 
    htmlTableHeaderCellElementToHTMLTableCellElement: htmlTableHeaderCellElementToHTMLTableCellElement, 
    readHTMLTableDataCellElement: readHTMLTableDataCellElement, 
    htmlTableDataCellElementToHTMLTableCellElement: htmlTableDataCellElementToHTMLTableCellElement, 
    readHTMLTableCellElement: readHTMLTableCellElement, 
    htmlTableCellElementToHTMLElement: htmlTableCellElementToHTMLElement, 
    readHTMLTableRowElement: readHTMLTableRowElement, 
    htmlTableRowElementToHTMLElement: htmlTableRowElementToHTMLElement, 
    readHTMLTableSectionElement: readHTMLTableSectionElement, 
    htmlTableSectionElementToHTMLElement: htmlTableSectionElementToHTMLElement, 
    readHTMLTableColElement: readHTMLTableColElement, 
    htmlTableColElementToHTMLElement: htmlTableColElementToHTMLElement, 
    readHTMLTableCaptionElement: readHTMLTableCaptionElement, 
    htmlTableCaptionElementToHTMLElement: htmlTableCaptionElementToHTMLElement, 
    readHTMLTableElement: readHTMLTableElement, 
    htmlTableElementToHTMLElement: htmlTableElementToHTMLElement, 
    readHTMLAreaElement: readHTMLAreaElement, 
    htmlAreaElementToHTMLElement: htmlAreaElementToHTMLElement, 
    readHTMLMapElement: readHTMLMapElement, 
    htmlMapElementToHTMLElement: htmlMapElementToHTMLElement, 
    readHTMLTrackElement: readHTMLTrackElement, 
    htmlTrackElementToHTMLElement: htmlTrackElementToHTMLElement, 
    readHTMLSourceElement: readHTMLSourceElement, 
    htmlSourceElementToHTMLElement: htmlSourceElementToHTMLElement, 
    readHTMLVideoElement: readHTMLVideoElement, 
    htmlVideoElementToHTMLMediaElement: htmlVideoElementToHTMLMediaElement, 
    readHTMLAudioElement: readHTMLAudioElement, 
    htmlAudioElementToHTMLMediaElement: htmlAudioElementToHTMLMediaElement, 
    readHTMLMediaElement: readHTMLMediaElement, 
    htmlMediaElementToHTMLElement: htmlMediaElementToHTMLElement, 
    readHTMLParamElement: readHTMLParamElement, 
    htmlParamElementToHTMLElement: htmlParamElementToHTMLElement, 
    readHTMLObjectElement: readHTMLObjectElement, 
    htmlObjectElementToHTMLElement: htmlObjectElementToHTMLElement, 
    readHTMLEmbedElement: readHTMLEmbedElement, 
    htmlEmbedElementToHTMLElement: htmlEmbedElementToHTMLElement, 
    readHTMLIFrameElement: readHTMLIFrameElement, 
    htmlIFrameElementToHTMLElement: htmlIFrameElementToHTMLElement, 
    readHTMLImageElement: readHTMLImageElement, 
    htmlImageElementToHTMLElement: htmlImageElementToHTMLElement, 
    readHTMLModElement: readHTMLModElement, 
    htmlModElementToHTMLElement: htmlModElementToHTMLElement, 
    readHTMLBRElement: readHTMLBRElement, 
    htmlBRElementToHTMLElement: htmlBRElementToHTMLElement, 
    readHTMLSpanElement: readHTMLSpanElement, 
    htmlSpanElementToHTMLElement: htmlSpanElementToHTMLElement, 
    readHTMLTimeElement: readHTMLTimeElement, 
    htmlTimeElementToHTMLElement: htmlTimeElementToHTMLElement, 
    readHTMLDataElement: readHTMLDataElement, 
    htmlDataElementToHTMLElement: htmlDataElementToHTMLElement, 
    readHTMLAnchorElement: readHTMLAnchorElement, 
    htmlAnchorElementToHTMLElement: htmlAnchorElementToHTMLElement, 
    readHTMLDivElement: readHTMLDivElement, 
    htmlDivElementToHTMLElement: htmlDivElementToHTMLElement, 
    readHTMLDListElement: readHTMLDListElement, 
    htmlDListElementToHTMLElement: htmlDListElementToHTMLElement, 
    readHTMLLIElement: readHTMLLIElement, 
    htmlLIElementToHTMLElement: htmlLIElementToHTMLElement, 
    readHTMLUListElement: readHTMLUListElement, 
    htmlUListElementToHTMLElement: htmlUListElementToHTMLElement, 
    readHTMLOListElement: readHTMLOListElement, 
    htmlOListElementToHTMLElement: htmlOListElementToHTMLElement, 
    readHTMLQuoteElement: readHTMLQuoteElement, 
    htmlQuoteElementToHTMLElement: htmlQuoteElementToHTMLElement, 
    readHTMLPreElement: readHTMLPreElement, 
    htmlPreElementToHTMLElement: htmlPreElementToHTMLElement, 
    readHTMLHRElement: readHTMLHRElement, 
    htmlHRElementToHTMLElement: htmlHRElementToHTMLElement, 
    readHTMLParagraphElement: readHTMLParagraphElement, 
    htmlParagraphElementToHTMLElement: htmlParagraphElementToHTMLElement, 
    readHTMLHeadingElement: readHTMLHeadingElement, 
    htmlHeadingElementToHTMLElement: htmlHeadingElementToHTMLElement, 
    readHTMLBodyElement: readHTMLBodyElement, 
    htmlBodyElementToHTMLElement: htmlBodyElementToHTMLElement, 
    readHTMLStyleElement: readHTMLStyleElement, 
    htmlStyleElementToHTMLElement: htmlStyleElementToHTMLElement, 
    readHTMLMetaElement: readHTMLMetaElement, 
    htmlMetaElementToHTMLElement: htmlMetaElementToHTMLElement, 
    readHTMLLinkElement: readHTMLLinkElement, 
    htmlLinkElementToHTMLElement: htmlLinkElementToHTMLElement, 
    readHTMLBaseElement: readHTMLBaseElement, 
    htmlBaseElementToHTMLElement: htmlBaseElementToHTMLElement, 
    readHTMLTitleElement: readHTMLTitleElement, 
    htmlTitleElementToHTMLElement: htmlTitleElementToHTMLElement, 
    readHTMLHeadElement: readHTMLHeadElement, 
    htmlHeadElementToHTMLElement: htmlHeadElementToHTMLElement, 
    readHTMLHtmlElement: readHTMLHtmlElement, 
    htmlHtmlElementToHTMLElement: htmlHtmlElementToHTMLElement, 
    readHTMLElement: readHTMLElement, 
    htmlElementToEventTarget: htmlElementToEventTarget, 
    htmlElementToNode: htmlElementToNode, 
    htmlElementToNonDocumentTypeChildNode: htmlElementToNonDocumentTypeChildNode, 
    htmlElementToParentNode: htmlElementToParentNode, 
    htmlElementToElement: htmlElementToElement, 
    readHTMLDocument: readHTMLDocument, 
    htmlDocumentToEventTarget: htmlDocumentToEventTarget, 
    htmlDocumentToNode: htmlDocumentToNode, 
    htmlDocumentToParentNode: htmlDocumentToParentNode, 
    htmlDocumentToNonElementParentNode: htmlDocumentToNonElementParentNode, 
    htmlDocumentToDocument: htmlDocumentToDocument, 
    windowToEventTarget: windowToEventTarget, 
    isForeignHTMLDocument: isForeignHTMLDocument, 
    isForeignHTMLElement: isForeignHTMLElement, 
    isForeignHTMLHtmlElement: isForeignHTMLHtmlElement, 
    isForeignHTMLHeadElement: isForeignHTMLHeadElement, 
    isForeignHTMLTitleElement: isForeignHTMLTitleElement, 
    isForeignHTMLBaseElement: isForeignHTMLBaseElement, 
    isForeignHTMLLinkElement: isForeignHTMLLinkElement, 
    isForeignHTMLMetaElement: isForeignHTMLMetaElement, 
    isForeignHTMLStyleElement: isForeignHTMLStyleElement, 
    isForeignHTMLBodyElement: isForeignHTMLBodyElement, 
    isForeignHTMLHeadingElement: isForeignHTMLHeadingElement, 
    isForeignHTMLParagraphElement: isForeignHTMLParagraphElement, 
    isForeignHTMLHRElement: isForeignHTMLHRElement, 
    isForeignHTMLPreElement: isForeignHTMLPreElement, 
    isForeignHTMLQuoteElement: isForeignHTMLQuoteElement, 
    isForeignHTMLOListElement: isForeignHTMLOListElement, 
    isForeignHTMLUListElement: isForeignHTMLUListElement, 
    isForeignHTMLLIElement: isForeignHTMLLIElement, 
    isForeignHTMLDListElement: isForeignHTMLDListElement, 
    isForeignHTMLDivElement: isForeignHTMLDivElement, 
    isForeignHTMLAnchorElement: isForeignHTMLAnchorElement, 
    isForeignHTMLDataElement: isForeignHTMLDataElement, 
    isForeignHTMLTimeElement: isForeignHTMLTimeElement, 
    isForeignHTMLSpanElement: isForeignHTMLSpanElement, 
    isForeignHTMLBRElement: isForeignHTMLBRElement, 
    isForeignHTMLModElement: isForeignHTMLModElement, 
    isForeignHTMLImageElement: isForeignHTMLImageElement, 
    isForeignHTMLIFrameElement: isForeignHTMLIFrameElement, 
    isForeignHTMLEmbedElement: isForeignHTMLEmbedElement, 
    isForeignHTMLObjectElement: isForeignHTMLObjectElement, 
    isForeignHTMLParamElement: isForeignHTMLParamElement, 
    isForeignHTMLSourceElement: isForeignHTMLSourceElement, 
    isForeignHTMLTrackElement: isForeignHTMLTrackElement, 
    isForeignHTMLMapElement: isForeignHTMLMapElement, 
    isForeignHTMLAreaElement: isForeignHTMLAreaElement, 
    isForeignHTMLTableElement: isForeignHTMLTableElement, 
    isForeignHTMLTableCaptionElement: isForeignHTMLTableCaptionElement, 
    isForeignHTMLTableColElement: isForeignHTMLTableColElement, 
    isForeignHTMLTableSectionElement: isForeignHTMLTableSectionElement, 
    isForeignHTMLTableRowElement: isForeignHTMLTableRowElement, 
    isForeignHTMLFormElement: isForeignHTMLFormElement, 
    isForeignHTMLLabelElement: isForeignHTMLLabelElement, 
    isForeignHTMLInputElement: isForeignHTMLInputElement, 
    isForeignHTMLButtonElement: isForeignHTMLButtonElement, 
    isForeignHTMLSelectElement: isForeignHTMLSelectElement, 
    isForeignHTMLDataListElement: isForeignHTMLDataListElement, 
    isForeignHTMLOptGroupElement: isForeignHTMLOptGroupElement, 
    isForeignHTMLOptionElement: isForeignHTMLOptionElement, 
    isForeignHTMLTextAreaElement: isForeignHTMLTextAreaElement, 
    isForeignHTMLKeygenElement: isForeignHTMLKeygenElement, 
    isForeignHTMLOutputElement: isForeignHTMLOutputElement, 
    isForeignHTMLProgressElement: isForeignHTMLProgressElement, 
    isForeignHTMLMeterElement: isForeignHTMLMeterElement, 
    isForeignHTMLFieldSetElement: isForeignHTMLFieldSetElement, 
    isForeignHTMLLegendElement: isForeignHTMLLegendElement, 
    isForeignHTMLScriptElement: isForeignHTMLScriptElement, 
    isForeignHTMLTemplateElement: isForeignHTMLTemplateElement, 
    isForeignHTMLCanvasElement: isForeignHTMLCanvasElement
};

},{"../DOM.Event.Types":84,"../DOM.Node.Types":97,"../Data.Either":114,"../Data.Foreign":128,"../Data.Foreign.Class":121,"../Prelude":203,"../Unsafe.Coerce":206,"./foreign":86}],88:[function(require,module,exports){
"use strict";

// module DOM.HTML.Window

exports.document = function (window) {
  return function () {
    return window.document;
  };
};

exports.navigator = function (window) {
  return function () {
    return window.navigator;
  };
};

exports.location = function (window) {
  return function () {
    return window.location;
  };
};

exports.innerWidth = function (window) {
  return function () {
    return window.innerWidth;
  };
};

exports.innerHeight = function (window) {
  return function () {
    return window.innerHeight;
  };
};

},{}],89:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var DOM = require("../DOM");
var DOM_HTML_Types = require("../DOM.HTML.Types");
module.exports = {
    innerHeight: $foreign.innerHeight, 
    innerWidth: $foreign.innerWidth, 
    location: $foreign.location, 
    navigator: $foreign.navigator, 
    document: $foreign.document
};

},{"../Control.Monad.Eff":54,"../DOM":98,"../DOM.HTML.Types":87,"./foreign":88}],90:[function(require,module,exports){
/* global window */
"use strict";

// module DOM.HTML

exports.window = function () {
  return window;
};

},{}],91:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var DOM = require("../DOM");
var DOM_HTML_Types = require("../DOM.HTML.Types");
module.exports = {
    window: $foreign.window
};

},{"../Control.Monad.Eff":54,"../DOM":98,"../DOM.HTML.Types":87,"./foreign":90}],92:[function(require,module,exports){
"use strict";

// module DOM.Node.Node

var getEffProp = function (name) {
  return function (node) {
    return function () {
      return node[name];
    };
  };
};

exports.nodeTypeIndex = function (node) {
  return node.nodeType;
};

exports.nodeName = function (node) {
  return node.nodeName;
};

exports.baseURI = getEffProp("baseURI");

exports.ownerDocument = getEffProp("ownerDocument");

exports.parentNode = getEffProp("parentNode");

exports.parentElement = getEffProp("parentElement");

exports.hasChildNodes = function (node) {
  return function () {
    return node.hasChildNodes();
  };
};

exports.childNodes = getEffProp("childNodes");

exports.firstChild = getEffProp("firstChild");

exports.lastChild = getEffProp("lastChild");

exports.previousSibling = getEffProp("previousSibling");

exports.nextSibling = getEffProp("nextSibling");

exports.nodeValue = getEffProp("nodeValue");

exports.setNodeValue = function (value) {
  return function (node) {
    return function () {
      node.nodeValue = value;
      return {};
    };
  };
};

exports.textContent = getEffProp("textContent");

exports.setTextContent = function (value) {
  return function (node) {
    return function () {
      node.textContent = value;
      return {};
    };
  };
};

exports.normalize = function (node) {
  return function () {
    node.normalize();
    return {};
  };
};

exports.clone = function (node) {
  return function () {
    return node.clone(false);
  };
};

exports.deepClone = function (node) {
  return function () {
    return node.clone(false);
  };
};

exports.isEqualNode = function (node1) {
  return function (node2) {
    return function () {
      return node1.isEqualNode(node2);
    };
  };
};

exports.compareDocumentPositionBits = function (node1) {
  return function (node2) {
    return function () {
      return node1.compareDocumentPosition(node2);
    };
  };
};

exports.contains = function (node1) {
  return function (node2) {
    return function () {
      return node1.contains(node2);
    };
  };
};

exports.lookupPrefix = function (prefix) {
  return function (node) {
    return function () {
      return node.lookupPrefix(prefix);
    };
  };
};

exports.lookupNamespaceURI = function (ns) {
  return function (node) {
    return function () {
      return node.lookupNamespaceURI(ns);
    };
  };
};

exports.isDefaultNamespace = function (ns) {
  return function (node) {
    return function () {
      return node.isDefaultNamespace(ns);
    };
  };
};

exports.insertBefore = function (node1) {
  return function (node2) {
    return function (parent) {
      return function () {
        return parent.insertBefore(node1, node2);
      };
    };
  };
};

exports.appendChild = function (node) {
  return function (parent) {
    return function () {
      return parent.appendChild(node);
    };
  };
};

exports.replaceChild = function (oldChild) {
  return function (newChild) {
    return function (parent) {
      return function () {
        return parent.replaceChild(oldChild, newChild);
      };
    };
  };
};

exports.removeChild = function (node) {
  return function (parent) {
    return function () {
      return parent.removeChild(node);
    };
  };
};

},{}],93:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Enum = require("../Data.Enum");
var Data_Nullable = require("../Data.Nullable");
var Data_Maybe_Unsafe = require("../Data.Maybe.Unsafe");
var DOM = require("../DOM");
var DOM_Node_NodeType = require("../DOM.Node.NodeType");
var DOM_Node_Types = require("../DOM.Node.Types");
var nodeType = function ($0) {
    return Data_Maybe_Unsafe.fromJust(Data_Enum.toEnum(DOM_Node_NodeType.enumNodeType)($foreign.nodeTypeIndex($0)));
};
module.exports = {
    nodeType: nodeType, 
    removeChild: $foreign.removeChild, 
    replaceChild: $foreign.replaceChild, 
    appendChild: $foreign.appendChild, 
    insertBefore: $foreign.insertBefore, 
    isDefaultNamespace: $foreign.isDefaultNamespace, 
    lookupNamespaceURI: $foreign.lookupNamespaceURI, 
    lookupPrefix: $foreign.lookupPrefix, 
    contains: $foreign.contains, 
    compareDocumentPositionBits: $foreign.compareDocumentPositionBits, 
    isEqualNode: $foreign.isEqualNode, 
    deepClone: $foreign.deepClone, 
    clone: $foreign.clone, 
    normalize: $foreign.normalize, 
    setTextContent: $foreign.setTextContent, 
    textContent: $foreign.textContent, 
    setNodeValue: $foreign.setNodeValue, 
    nodeValue: $foreign.nodeValue, 
    nextSibling: $foreign.nextSibling, 
    previousSibling: $foreign.previousSibling, 
    lastChild: $foreign.lastChild, 
    firstChild: $foreign.firstChild, 
    childNodes: $foreign.childNodes, 
    hasChildNodes: $foreign.hasChildNodes, 
    parentElement: $foreign.parentElement, 
    parentNode: $foreign.parentNode, 
    ownerDocument: $foreign.ownerDocument, 
    baseURI: $foreign.baseURI, 
    nodeName: $foreign.nodeName, 
    nodeTypeIndex: $foreign.nodeTypeIndex
};

},{"../Control.Monad.Eff":54,"../DOM":98,"../DOM.Node.NodeType":94,"../DOM.Node.Types":97,"../Data.Enum":115,"../Data.Maybe.Unsafe":151,"../Data.Nullable":162,"../Prelude":203,"./foreign":92}],94:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Enum = require("../Data.Enum");
var ElementNode = (function () {
    function ElementNode() {

    };
    ElementNode.value = new ElementNode();
    return ElementNode;
})();
var AttributeNode = (function () {
    function AttributeNode() {

    };
    AttributeNode.value = new AttributeNode();
    return AttributeNode;
})();
var TextNode = (function () {
    function TextNode() {

    };
    TextNode.value = new TextNode();
    return TextNode;
})();
var CDATASectionNode = (function () {
    function CDATASectionNode() {

    };
    CDATASectionNode.value = new CDATASectionNode();
    return CDATASectionNode;
})();
var EntityReferenceNode = (function () {
    function EntityReferenceNode() {

    };
    EntityReferenceNode.value = new EntityReferenceNode();
    return EntityReferenceNode;
})();
var EntityNode = (function () {
    function EntityNode() {

    };
    EntityNode.value = new EntityNode();
    return EntityNode;
})();
var ProcessingInstructionNode = (function () {
    function ProcessingInstructionNode() {

    };
    ProcessingInstructionNode.value = new ProcessingInstructionNode();
    return ProcessingInstructionNode;
})();
var CommentNode = (function () {
    function CommentNode() {

    };
    CommentNode.value = new CommentNode();
    return CommentNode;
})();
var DocumentNode = (function () {
    function DocumentNode() {

    };
    DocumentNode.value = new DocumentNode();
    return DocumentNode;
})();
var DocumentTypeNode = (function () {
    function DocumentTypeNode() {

    };
    DocumentTypeNode.value = new DocumentTypeNode();
    return DocumentTypeNode;
})();
var DocumentFragmentNode = (function () {
    function DocumentFragmentNode() {

    };
    DocumentFragmentNode.value = new DocumentFragmentNode();
    return DocumentFragmentNode;
})();
var NotationNode = (function () {
    function NotationNode() {

    };
    NotationNode.value = new NotationNode();
    return NotationNode;
})();
var toEnumNodeType = function (v) {
    if (v === 1) {
        return new Data_Maybe.Just(ElementNode.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(AttributeNode.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(TextNode.value);
    };
    if (v === 4) {
        return new Data_Maybe.Just(CDATASectionNode.value);
    };
    if (v === 5) {
        return new Data_Maybe.Just(EntityReferenceNode.value);
    };
    if (v === 6) {
        return new Data_Maybe.Just(EntityNode.value);
    };
    if (v === 7) {
        return new Data_Maybe.Just(ProcessingInstructionNode.value);
    };
    if (v === 8) {
        return new Data_Maybe.Just(CommentNode.value);
    };
    if (v === 9) {
        return new Data_Maybe.Just(DocumentNode.value);
    };
    if (v === 10) {
        return new Data_Maybe.Just(DocumentTypeNode.value);
    };
    if (v === 11) {
        return new Data_Maybe.Just(DocumentFragmentNode.value);
    };
    if (v === 12) {
        return new Data_Maybe.Just(NotationNode.value);
    };
    return Data_Maybe.Nothing.value;
};
var fromEnumNodeType = function (v) {
    if (v instanceof ElementNode) {
        return 1;
    };
    if (v instanceof AttributeNode) {
        return 2;
    };
    if (v instanceof TextNode) {
        return 3;
    };
    if (v instanceof CDATASectionNode) {
        return 4;
    };
    if (v instanceof EntityReferenceNode) {
        return 5;
    };
    if (v instanceof EntityNode) {
        return 6;
    };
    if (v instanceof ProcessingInstructionNode) {
        return 7;
    };
    if (v instanceof CommentNode) {
        return 8;
    };
    if (v instanceof DocumentNode) {
        return 9;
    };
    if (v instanceof DocumentTypeNode) {
        return 10;
    };
    if (v instanceof DocumentFragmentNode) {
        return 11;
    };
    if (v instanceof NotationNode) {
        return 12;
    };
    throw new Error("Failed pattern match at DOM.Node.NodeType line 68, column 1 - line 69, column 1: " + [ v.constructor.name ]);
};
var eqNodeType = new Prelude.Eq(function (v) {
    return function (v1) {
        if (v instanceof ElementNode && v1 instanceof ElementNode) {
            return true;
        };
        if (v instanceof AttributeNode && v1 instanceof AttributeNode) {
            return true;
        };
        if (v instanceof TextNode && v1 instanceof TextNode) {
            return true;
        };
        if (v instanceof CDATASectionNode && v1 instanceof CDATASectionNode) {
            return true;
        };
        if (v instanceof EntityReferenceNode && v1 instanceof EntityReferenceNode) {
            return true;
        };
        if (v instanceof EntityNode && v1 instanceof EntityNode) {
            return true;
        };
        if (v instanceof ProcessingInstructionNode && v1 instanceof ProcessingInstructionNode) {
            return true;
        };
        if (v instanceof CommentNode && v1 instanceof CommentNode) {
            return true;
        };
        if (v instanceof DocumentNode && v1 instanceof DocumentNode) {
            return true;
        };
        if (v instanceof DocumentTypeNode && v1 instanceof DocumentTypeNode) {
            return true;
        };
        if (v instanceof DocumentFragmentNode && v1 instanceof DocumentFragmentNode) {
            return true;
        };
        if (v instanceof NotationNode && v1 instanceof NotationNode) {
            return true;
        };
        return false;
    };
});
var ordNodeType = new Prelude.Ord(function () {
    return eqNodeType;
}, function (x) {
    return function (y) {
        return Prelude.compare(Prelude.ordInt)(fromEnumNodeType(x))(fromEnumNodeType(y));
    };
});
var boundedNodeType = new Prelude.Bounded(ElementNode.value, NotationNode.value);
var boundedOrdNodeType = new Prelude.BoundedOrd(function () {
    return boundedNodeType;
}, function () {
    return ordNodeType;
});
var enumNodeType = new Data_Enum.Enum(function () {
    return boundedNodeType;
}, 12, fromEnumNodeType, Data_Enum.defaultPred(toEnumNodeType)(fromEnumNodeType), Data_Enum.defaultSucc(toEnumNodeType)(fromEnumNodeType), toEnumNodeType);
module.exports = {
    ElementNode: ElementNode, 
    AttributeNode: AttributeNode, 
    TextNode: TextNode, 
    CDATASectionNode: CDATASectionNode, 
    EntityReferenceNode: EntityReferenceNode, 
    EntityNode: EntityNode, 
    ProcessingInstructionNode: ProcessingInstructionNode, 
    CommentNode: CommentNode, 
    DocumentNode: DocumentNode, 
    DocumentTypeNode: DocumentTypeNode, 
    DocumentFragmentNode: DocumentFragmentNode, 
    NotationNode: NotationNode, 
    eqNodeType: eqNodeType, 
    ordNodeType: ordNodeType, 
    boundedNodeType: boundedNodeType, 
    boundedOrdNodeType: boundedOrdNodeType, 
    enumNodeType: enumNodeType
};

},{"../Data.Enum":115,"../Data.Maybe":152,"../Prelude":203}],95:[function(require,module,exports){
"use strict";

// module DOM.Node.ParentNode

var getEffProp = function (name) {
  return function (node) {
    return function () {
      return node[name];
    };
  };
};

exports.children = getEffProp("children");

exports.firstElementChild = getEffProp("firstElementChild");

exports.lastElementChild = getEffProp("lastElementChild");

exports.childElementCount = getEffProp("childElementCount");

exports.querySelector = function (selector) {
  return function (node) {
    return function () {
      return node.querySelector(selector);
    };
  };
};

exports.querySelectorAll = function (selector) {
  return function (node) {
    return function () {
      return node.querySelectorAll(selector);
    };
  };
};

},{}],96:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Nullable = require("../Data.Nullable");
var DOM = require("../DOM");
var DOM_Node_Types = require("../DOM.Node.Types");
module.exports = {
    querySelectorAll: $foreign.querySelectorAll, 
    querySelector: $foreign.querySelector, 
    childElementCount: $foreign.childElementCount, 
    lastElementChild: $foreign.lastElementChild, 
    firstElementChild: $foreign.firstElementChild, 
    children: $foreign.children
};

},{"../Control.Monad.Eff":54,"../DOM":98,"../DOM.Node.Types":97,"../Data.Nullable":162,"./foreign":95}],97:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var DOM_Event_Types = require("../DOM.Event.Types");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var ElementId = function (x) {
    return x;
};
var textToNode = Unsafe_Coerce.unsafeCoerce;
var processingInstructionToNode = Unsafe_Coerce.unsafeCoerce;
var elementToParentNode = Unsafe_Coerce.unsafeCoerce;
var elementToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var elementToNode = Unsafe_Coerce.unsafeCoerce;
var elementToEventTarget = Unsafe_Coerce.unsafeCoerce;
var documentTypeToNode = Unsafe_Coerce.unsafeCoerce;
var documentToParentNode = Unsafe_Coerce.unsafeCoerce;
var documentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var documentToNode = Unsafe_Coerce.unsafeCoerce;
var documentToEventTarget = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToParentNode = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToNode = Unsafe_Coerce.unsafeCoerce;
var commentToNode = Unsafe_Coerce.unsafeCoerce;
var characterDataToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    ElementId: ElementId, 
    documentTypeToNode: documentTypeToNode, 
    documentFragmentToNode: documentFragmentToNode, 
    documentFragmentToParentNode: documentFragmentToParentNode, 
    documentFragmentToNonElementParentNode: documentFragmentToNonElementParentNode, 
    processingInstructionToNode: processingInstructionToNode, 
    commentToNode: commentToNode, 
    textToNode: textToNode, 
    characterDataToNonDocumentTypeChildNode: characterDataToNonDocumentTypeChildNode, 
    elementToEventTarget: elementToEventTarget, 
    elementToNode: elementToNode, 
    elementToNonDocumentTypeChildNode: elementToNonDocumentTypeChildNode, 
    elementToParentNode: elementToParentNode, 
    documentToEventTarget: documentToEventTarget, 
    documentToNode: documentToNode, 
    documentToParentNode: documentToParentNode, 
    documentToNonElementParentNode: documentToNonElementParentNode
};

},{"../DOM.Event.Types":84,"../Prelude":203,"../Unsafe.Coerce":206}],98:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
module.exports = {};

},{}],99:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Array.ST

exports.runSTArray = function (f) {
  return f;
};

exports.emptySTArray = function () {
  return [];
};

exports.peekSTArrayImpl = function (just) {
  return function (nothing) {
    return function (xs) {
      return function (i) {
        return function () {
          return i >= 0 && i < xs.length ? just(xs[i]) : nothing;
        };
      };
    };
  };
};

exports.pokeSTArray = function (xs) {
  return function (i) {
    return function (a) {
      return function () {
        var ret = i >= 0 && i < xs.length;
        if (ret) xs[i] = a;
        return ret;
      };
    };
  };
};

exports.pushAllSTArray = function (xs) {
  return function (as) {
    return function () {
      return xs.push.apply(xs, as);
    };
  };
};

exports.spliceSTArray = function (xs) {
  return function (i) {
    return function (howMany) {
      return function (bs) {
        return function () {
          return xs.splice.apply(xs, [i, howMany].concat(bs));
        };
      };
    };
  };
};

exports.copyImpl = function (xs) {
  return function () {
    return xs.slice();
  };
};

exports.toAssocArray = function (xs) {
  return function () {
    var n = xs.length;
    var as = new Array(n);
    for (var i = 0; i < n; i++) as[i] = { value: xs[i], index: i };
    return as;
  };
};

},{}],100:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_ST = require("../Control.Monad.ST");
var Data_Maybe = require("../Data.Maybe");
var thaw = $foreign.copyImpl;
var pushSTArray = function (arr) {
    return function (a) {
        return $foreign.pushAllSTArray(arr)([ a ]);
    };
};
var peekSTArray = $foreign.peekSTArrayImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var freeze = $foreign.copyImpl;
module.exports = {
    thaw: thaw, 
    freeze: freeze, 
    pushSTArray: pushSTArray, 
    peekSTArray: peekSTArray, 
    toAssocArray: $foreign.toAssocArray, 
    spliceSTArray: $foreign.spliceSTArray, 
    pushAllSTArray: $foreign.pushAllSTArray, 
    pokeSTArray: $foreign.pokeSTArray, 
    emptySTArray: $foreign.emptySTArray, 
    runSTArray: $foreign.runSTArray
};

},{"../Control.Monad.Eff":54,"../Control.Monad.ST":67,"../Data.Maybe":152,"../Prelude":203,"./foreign":99}],101:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Array

//------------------------------------------------------------------------------
// Array creation --------------------------------------------------------------
//------------------------------------------------------------------------------

exports.range = function (start) {
  return function (end) {
    var step = start > end ? -1 : 1;
    var result = [];
    for (var i = start, n = 0; i !== end; i += step) {
      result[n++] = i;
    }
    result[n] = i;
    return result;
  };
};

exports.replicate = function (n) {
  return function (v) {
    if (n < 1) return [];
    var r = new Array(n);
    for (var i = 0; i < n; i++) r[i] = v;
    return r;
  };
};

//------------------------------------------------------------------------------
// Array size ------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.length = function (xs) {
  return xs.length;
};

//------------------------------------------------------------------------------
// Extending arrays ------------------------------------------------------------
//------------------------------------------------------------------------------

exports.cons = function (e) {
  return function (l) {
    return [e].concat(l);
  };
};

exports.snoc = function (l) {
  return function (e) {
    var l1 = l.slice();
    l1.push(e);
    return l1;
  };
};

//------------------------------------------------------------------------------
// Non-indexed reads -----------------------------------------------------------
//------------------------------------------------------------------------------

exports["uncons'"] = function (empty) {
  return function (next) {
    return function (xs) {
      return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
    };
  };
};

//------------------------------------------------------------------------------
// Indexed operations ----------------------------------------------------------
//------------------------------------------------------------------------------

exports.indexImpl = function (just) {
  return function (nothing) {
    return function (xs) {
      return function (i) {
        return i < 0 || i >= xs.length ? nothing :  just(xs[i]);
      };
    };
  };
};

exports.findIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = 0, l = xs.length; i < l; i++) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

exports.findLastIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = xs.length - 1; i >= 0; i--) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

exports._insertAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i > l.length) return nothing;
          var l1 = l.slice();
          l1.splice(i, 0, a);
          return just(l1);
        };
      };
    };
  };
};

exports._deleteAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (l) {
        if (i < 0 || i >= l.length) return nothing;
        var l1 = l.slice();
        l1.splice(i, 1);
        return just(l1);
      };
    };
  };
};

exports._updateAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i >= l.length) return nothing;
          var l1 = l.slice();
          l1[i] = a;
          return just(l1);
        };
      };
    };
  };
};

//------------------------------------------------------------------------------
// Transformations -------------------------------------------------------------
//------------------------------------------------------------------------------

exports.reverse = function (l) {
  return l.slice().reverse();
};

exports.concat = function (xss) {
  var result = [];
  for (var i = 0, l = xss.length; i < l; i++) {
    var xs = xss[i];
    for (var j = 0, m = xs.length; j < m; j++) {
      result.push(xs[j]);
    }
  }
  return result;
};

exports.filter = function (f) {
  return function (xs) {
    return xs.filter(f);
  };
};

exports.partition = function (f) {
  return function (xs) {
    var yes = [];
    var no  = [];
    for (var i = 0; i < xs.length; i++) {
      var x = xs[i];
      if (f(x))
        yes.push(x);
      else
        no.push(x);
    }
    return { yes: yes, no: no };
  };
};

//------------------------------------------------------------------------------
// Sorting ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.sortImpl = function (f) {
  return function (l) {
    /* jshint maxparams: 2 */
    return l.slice().sort(function (x, y) {
      return f(x)(y);
    });
  };
};

//------------------------------------------------------------------------------
// Subarrays -------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.slice = function (s) {
  return function (e) {
    return function (l) {
      return l.slice(s, e);
    };
  };
};

exports.drop = function (n) {
  return function (l) {
    return n < 1 ? l : l.slice(n);
  };
};

//------------------------------------------------------------------------------
// Zipping ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.zipWith = function (f) {
  return function (xs) {
    return function (ys) {
      var l = xs.length < ys.length ? xs.length : ys.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(xs[i])(ys[i]);
      }
      return result;
    };
  };
};

},{}],102:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Lazy = require("../Control.Lazy");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_Plus = require("../Control.Plus");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Maybe_Unsafe = require("../Data.Maybe.Unsafe");
var $colon = $foreign.cons;
var $dot$dot = $foreign.range;
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(Data_Traversable.traversableArray)(dictApplicative)($foreign.zipWith(f)(xs)(ys));
            };
        };
    };
};
var zip = $foreign.zipWith(Data_Tuple.Tuple.create);
var updateAt = $foreign._updateAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unzip = $foreign["uncons'"](function (v) {
    return new Data_Tuple.Tuple([  ], [  ]);
})(function (v) {
    return function (ts) {
        var $36 = unzip(ts);
        return new Data_Tuple.Tuple($colon(v.value0)($36.value0), $colon(v.value1)($36.value1));
    };
});
var uncons = $foreign["uncons'"](Prelude["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (xs) {
        return new Data_Maybe.Just({
            head: x, 
            tail: xs
        });
    };
});
var take = $foreign.slice(0);
var tail = $foreign["uncons'"](Prelude["const"](Data_Maybe.Nothing.value))(function (v) {
    return function (xs) {
        return new Data_Maybe.Just(xs);
    };
});
var span = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_xs) {
            var acc = __copy_acc;
            var xs = __copy_xs;
            tco: while (true) {
                var $42 = uncons(xs);
                if ($42 instanceof Data_Maybe.Just && p($42.value0.head)) {
                    var __tco_acc = $colon($42.value0.head)(acc);
                    acc = __tco_acc;
                    xs = $42.value0.tail;
                    continue tco;
                };
                return {
                    init: $foreign.reverse(acc), 
                    rest: xs
                };
            };
        };
    };
    return go([  ]);
};
var takeWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).init;
    };
};
var sortBy = function (comp) {
    return function (xs) {
        var comp$prime = function (x) {
            return function (y) {
                var $46 = comp(x)(y);
                if ($46 instanceof Prelude.GT) {
                    return 1;
                };
                if ($46 instanceof Prelude.EQ) {
                    return 0;
                };
                if ($46 instanceof Prelude.LT) {
                    return -1;
                };
                throw new Error("Failed pattern match at Data.Array line 417, column 15 - line 422, column 1: " + [ $46.constructor.name ]);
            };
        };
        return $foreign.sortImpl(comp$prime)(xs);
    };
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Prelude.compare(dictOrd))(xs);
    };
};
var singleton = function (a) {
    return [ a ];
};
var replicateM = function (dictMonad) {
    return function (n) {
        return function (m) {
            if (n < 1) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())([  ]);
            };
            if (Prelude.otherwise) {
                return Data_Traversable.sequence(Data_Traversable.traversableArray)(dictMonad["__superclass_Prelude.Applicative_0"]())($foreign.replicate(n)(m));
            };
            throw new Error("Failed pattern match at Data.Array line 138, column 1 - line 145, column 1: " + [ n.constructor.name, m.constructor.name ]);
        };
    };
};
var $$null = function (xs) {
    return $foreign.length(xs) === 0;
};
var nubBy = function (eq) {
    return function (xs) {
        var $49 = uncons(xs);
        if ($49 instanceof Data_Maybe.Just) {
            return $colon($49.value0.head)(nubBy(eq)($foreign.filter(function (y) {
                return !eq($49.value0.head)(y);
            })($49.value0.tail)));
        };
        if ($49 instanceof Data_Maybe.Nothing) {
            return [  ];
        };
        throw new Error("Failed pattern match at Data.Array line 500, column 15 - line 507, column 1: " + [ $49.constructor.name ]);
    };
};
var nub = function (dictEq) {
    return nubBy(Prelude.eq(dictEq));
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Prelude["<*>"]((dictAlternative["__superclass_Prelude.Applicative_0"]())["__superclass_Prelude.Apply_0"]())(Prelude["<$>"](((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())["__superclass_Prelude.Functor_0"]())($colon)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt["<|>"]((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())(some(dictAlternative)(dictLazy)(v))(Prelude.pure(dictAlternative["__superclass_Prelude.Applicative_0"]())([  ]));
        };
    };
};
var insertAt = $foreign._insertAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var init = function (xs) {
    if ($$null(xs)) {
        return Data_Maybe.Nothing.value;
    };
    if (Prelude.otherwise) {
        return new Data_Maybe.Just($foreign.slice(0)($foreign.length(xs) - 1)(xs));
    };
    throw new Error("Failed pattern match at Data.Array line 228, column 1 - line 245, column 1: " + [ xs.constructor.name ]);
};
var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var $bang$bang = index;
var last = function (xs) {
    return $bang$bang(xs)($foreign.length(xs) - 1);
};
var modifyAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                return updateAt(i)(f(x))(xs);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)($bang$bang(xs)(i));
        };
    };
};
var head = $foreign["uncons'"](Prelude["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (v) {
        return new Data_Maybe.Just(x);
    };
});
var groupBy = function (op) {
    var go = function (__copy_acc) {
        return function (__copy_xs) {
            var acc = __copy_acc;
            var xs = __copy_xs;
            tco: while (true) {
                var $54 = uncons(xs);
                if ($54 instanceof Data_Maybe.Just) {
                    var sp = span(op($54.value0.head))($54.value0.tail);
                    var __tco_acc = $colon($colon($54.value0.head)(sp.init))(acc);
                    acc = __tco_acc;
                    xs = sp.rest;
                    continue tco;
                };
                if ($54 instanceof Data_Maybe.Nothing) {
                    return $foreign.reverse(acc);
                };
                throw new Error("Failed pattern match at Data.Array line 488, column 15 - line 494, column 1: " + [ $54.constructor.name ]);
            };
        };
    };
    return go([  ]);
};
var group = function (dictEq) {
    return function (xs) {
        return groupBy(Prelude.eq(dictEq))(xs);
    };
};
var group$prime = function (dictOrd) {
    return function ($68) {
        return group(dictOrd["__superclass_Prelude.Eq_0"]())(sort(dictOrd)($68));
    };
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (a) {
            return $foreign["uncons'"](function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(a);
            })(function (b) {
                return function (bs) {
                    return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(f(a)(b))(function (a$prime) {
                        return foldM(dictMonad)(f)(a$prime)(bs);
                    });
                };
            });
        };
    };
};
var findLastIndex = $foreign.findLastIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var insertBy = function (cmp) {
    return function (x) {
        return function (ys) {
            var i = Data_Maybe.maybe(0)(function (v) {
                return v + 1 | 0;
            })(findLastIndex(function (y) {
                return Prelude["=="](Prelude.eqOrdering)(cmp(x)(y))(Prelude.GT.value);
            })(ys));
            return Data_Maybe_Unsafe.fromJust(insertAt(i)(x)(ys));
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Prelude.compare(dictOrd));
};
var findIndex = $foreign.findIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var intersectBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return $foreign.filter(function (x) {
                return Data_Maybe.isJust(findIndex(eq(x))(ys));
            })(xs);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Prelude.eq(dictEq));
};
var filterM = function (dictMonad) {
    return function (p) {
        return $foreign["uncons'"](function (v) {
            return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())([  ]);
        })(function (x) {
            return function (xs) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(p(x))(function (v) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(filterM(dictMonad)(p)(xs))(function (v1) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())((function () {
                            if (v) {
                                return $colon(x)(v1);
                            };
                            if (!v) {
                                return v1;
                            };
                            throw new Error("Failed pattern match at Data.Array line 390, column 5 - line 396, column 1: " + [ v.constructor.name ]);
                        })());
                    });
                });
            };
        });
    };
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Prelude["=="](dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Prelude["=="](dictEq)(v)(x);
        });
    };
};
var dropWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).rest;
    };
};
var deleteAt = $foreign._deleteAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var deleteBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2.length === 0) {
                return [  ];
            };
            return Data_Maybe.maybe(v2)(function (i) {
                return Data_Maybe_Unsafe.fromJust(deleteAt(i)(v2));
            })(findIndex(v(v1))(v2));
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Prelude["++"](Prelude.semigroupArray)(xs)(Data_Foldable.foldl(Data_Foldable.foldableArray)(Prelude.flip(deleteBy(eq)))(nubBy(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Prelude["=="](dictEq));
};
var $$delete = function (dictEq) {
    return deleteBy(Prelude.eq(dictEq));
};
var $bslash$bslash = function (dictEq) {
    return function (xs) {
        return function (ys) {
            if ($$null(xs)) {
                return [  ];
            };
            if (Prelude.otherwise) {
                return $foreign["uncons'"](Prelude["const"](xs))(function (y) {
                    return function (ys2) {
                        return $bslash$bslash(dictEq)($$delete(dictEq)(y)(xs))(ys2);
                    };
                })(ys);
            };
            throw new Error("Failed pattern match at Data.Array line 532, column 1 - line 536, column 1: " + [ xs.constructor.name, ys.constructor.name ]);
        };
    };
};
var concatMap = Prelude.flip(Prelude.bind(Prelude.bindArray));
var mapMaybe = function (f) {
    return concatMap(function ($69) {
        return Data_Maybe.maybe([  ])(singleton)(f($69));
    });
};
var catMaybes = mapMaybe(Prelude.id(Prelude.categoryFn));
var alterAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                var $66 = f(x);
                if ($66 instanceof Data_Maybe.Nothing) {
                    return deleteAt(i)(xs);
                };
                if ($66 instanceof Data_Maybe.Just) {
                    return updateAt(i)($66.value0)(xs);
                };
                throw new Error("Failed pattern match at Data.Array line 350, column 10 - line 359, column 1: " + [ $66.constructor.name ]);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)($bang$bang(xs)(i));
        };
    };
};
module.exports = {
    foldM: foldM, 
    unzip: unzip, 
    zip: zip, 
    zipWithA: zipWithA, 
    intersectBy: intersectBy, 
    intersect: intersect, 
    "\\\\": $bslash$bslash, 
    deleteBy: deleteBy, 
    "delete": $$delete, 
    unionBy: unionBy, 
    union: union, 
    nubBy: nubBy, 
    nub: nub, 
    groupBy: groupBy, 
    "group'": group$prime, 
    group: group, 
    span: span, 
    dropWhile: dropWhile, 
    takeWhile: takeWhile, 
    take: take, 
    sortBy: sortBy, 
    sort: sort, 
    catMaybes: catMaybes, 
    mapMaybe: mapMaybe, 
    filterM: filterM, 
    concatMap: concatMap, 
    alterAt: alterAt, 
    modifyAt: modifyAt, 
    updateAt: updateAt, 
    deleteAt: deleteAt, 
    insertAt: insertAt, 
    findLastIndex: findLastIndex, 
    findIndex: findIndex, 
    elemLastIndex: elemLastIndex, 
    elemIndex: elemIndex, 
    index: index, 
    "!!": $bang$bang, 
    uncons: uncons, 
    init: init, 
    tail: tail, 
    last: last, 
    head: head, 
    insertBy: insertBy, 
    insert: insert, 
    ":": $colon, 
    "null": $$null, 
    many: many, 
    some: some, 
    replicateM: replicateM, 
    "..": $dot$dot, 
    singleton: singleton, 
    zipWith: $foreign.zipWith, 
    drop: $foreign.drop, 
    slice: $foreign.slice, 
    partition: $foreign.partition, 
    filter: $foreign.filter, 
    concat: $foreign.concat, 
    reverse: $foreign.reverse, 
    snoc: $foreign.snoc, 
    cons: $foreign.cons, 
    length: $foreign.length, 
    replicate: $foreign.replicate, 
    range: $foreign.range
};

},{"../Control.Alt":26,"../Control.Alternative":27,"../Control.Lazy":37,"../Control.MonadPlus":76,"../Control.Plus":77,"../Data.Foldable":120,"../Data.Functor.Invariant":133,"../Data.Maybe":152,"../Data.Maybe.Unsafe":151,"../Data.Monoid":159,"../Data.Traversable":170,"../Data.Tuple":171,"../Prelude":203,"./foreign":101}],103:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Apply = require("../Control.Apply");
var Data_Monoid = require("../Data.Monoid");
var Data_Monoid_Disj = require("../Data.Monoid.Disj");
var Data_Monoid_Conj = require("../Data.Monoid.Conj");
var Data_Monoid_Endo = require("../Data.Monoid.Endo");
var Data_Monoid_Dual = require("../Data.Monoid.Dual");
var Bifoldable = function (bifoldMap, bifoldl, bifoldr) {
    this.bifoldMap = bifoldMap;
    this.bifoldl = bifoldl;
    this.bifoldr = bifoldr;
};
var bifoldr = function (dict) {
    return dict.bifoldr;
};
var bitraverse_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return bifoldr(dictBifoldable)(function ($18) {
                    return Control_Apply["*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(f($18));
                })(function ($19) {
                    return Control_Apply["*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(g($19));
                })(Prelude.pure(dictApplicative)(Prelude.unit));
            };
        };
    };
};
var bifor_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse_(dictBifoldable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};
var bisequence_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return bitraverse_(dictBifoldable)(dictApplicative)(Prelude.id(Prelude.categoryFn))(Prelude.id(Prelude.categoryFn));
    };
};
var bifoldl = function (dict) {
    return dict.bifoldl;
};
var bifoldMapDefaultR = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return function (p) {
                    return bifoldr(dictBifoldable)(function ($20) {
                        return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(f($20));
                    })(function ($21) {
                        return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(g($21));
                    })(Data_Monoid.mempty(dictMonoid))(p);
                };
            };
        };
    };
};
var bifoldMapDefaultL = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return function (p) {
                    return bifoldl(dictBifoldable)(function (m) {
                        return function (a) {
                            return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(m)(f(a));
                        };
                    })(function (m) {
                        return function (b) {
                            return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(m)(g(b));
                        };
                    })(Data_Monoid.mempty(dictMonoid))(p);
                };
            };
        };
    };
};
var bifoldMap = function (dict) {
    return dict.bifoldMap;
};
var bifoldlDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Monoid_Endo.runEndo(Data_Monoid_Dual.runDual(bifoldMap(dictBifoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo))(function ($22) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Prelude.flip(f)($22)));
                    })(function ($23) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Prelude.flip(g)($23)));
                    })(p)))(z);
                };
            };
        };
    };
};
var bifoldrDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Monoid_Endo.runEndo(bifoldMap(dictBifoldable)(Data_Monoid_Endo.monoidEndo)(function ($24) {
                        return Data_Monoid_Endo.Endo(f($24));
                    })(function ($25) {
                        return Data_Monoid_Endo.Endo(g($25));
                    })(p))(z);
                };
            };
        };
    };
};
var bifold = function (dictBifoldable) {
    return function (dictMonoid) {
        return bifoldMap(dictBifoldable)(dictMonoid)(Prelude.id(Prelude.categoryFn))(Prelude.id(Prelude.categoryFn));
    };
};
var biany = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                return function ($26) {
                    return Data_Monoid_Disj.runDisj(bifoldMap(dictBifoldable)(Data_Monoid_Disj.monoidDisj(dictBooleanAlgebra))(function ($27) {
                        return Data_Monoid_Disj.Disj(p($27));
                    })(function ($28) {
                        return Data_Monoid_Disj.Disj(q($28));
                    })($26));
                };
            };
        };
    };
};
var biall = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                return function ($29) {
                    return Data_Monoid_Conj.runConj(bifoldMap(dictBifoldable)(Data_Monoid_Conj.monoidConj(dictBooleanAlgebra))(function ($30) {
                        return Data_Monoid_Conj.Conj(p($30));
                    })(function ($31) {
                        return Data_Monoid_Conj.Conj(q($31));
                    })($29));
                };
            };
        };
    };
};
module.exports = {
    Bifoldable: Bifoldable, 
    biall: biall, 
    biany: biany, 
    bisequence_: bisequence_, 
    bifor_: bifor_, 
    bitraverse_: bitraverse_, 
    bifold: bifold, 
    bifoldMapDefaultL: bifoldMapDefaultL, 
    bifoldMapDefaultR: bifoldMapDefaultR, 
    bifoldlDefault: bifoldlDefault, 
    bifoldrDefault: bifoldrDefault, 
    bifoldMap: bifoldMap, 
    bifoldl: bifoldl, 
    bifoldr: bifoldr
};

},{"../Control.Apply":28,"../Data.Monoid":159,"../Data.Monoid.Conj":154,"../Data.Monoid.Disj":155,"../Data.Monoid.Dual":156,"../Data.Monoid.Endo":157,"../Prelude":203}],104:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Bifunctor = function (bimap) {
    this.bimap = bimap;
};
var bimap = function (dict) {
    return dict.bimap;
};
var lmap = function (dictBifunctor) {
    return function (f) {
        return bimap(dictBifunctor)(f)(Prelude.id(Prelude.categoryFn));
    };
};
var rmap = function (dictBifunctor) {
    return bimap(dictBifunctor)(Prelude.id(Prelude.categoryFn));
};
module.exports = {
    Bifunctor: Bifunctor, 
    rmap: rmap, 
    lmap: lmap, 
    bimap: bimap
};

},{"../Prelude":203}],105:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Bifoldable = require("../Data.Bifoldable");
var Data_Bifunctor = require("../Data.Bifunctor");
var Bitraversable = function (__superclass_Data$dotBifoldable$dotBifoldable_1, __superclass_Data$dotBifunctor$dotBifunctor_0, bisequence, bitraverse) {
    this["__superclass_Data.Bifoldable.Bifoldable_1"] = __superclass_Data$dotBifoldable$dotBifoldable_1;
    this["__superclass_Data.Bifunctor.Bifunctor_0"] = __superclass_Data$dotBifunctor$dotBifunctor_0;
    this.bisequence = bisequence;
    this.bitraverse = bitraverse;
};
var bitraverse = function (dict) {
    return dict.bitraverse;
};
var bisequenceDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return bitraverse(dictBitraversable)(dictApplicative)(Prelude.id(Prelude.categoryFn))(Prelude.id(Prelude.categoryFn))(t);
        };
    };
};
var bisequence = function (dict) {
    return dict.bisequence;
};
var bitraverseDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return function (t) {
                    return bisequence(dictBitraversable)(dictApplicative)(Data_Bifunctor.bimap(dictBitraversable["__superclass_Data.Bifunctor.Bifunctor_0"]())(f)(g)(t));
                };
            };
        };
    };
};
var bifor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse(dictBitraversable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};
module.exports = {
    Bitraversable: Bitraversable, 
    bifor: bifor, 
    bisequenceDefault: bisequenceDefault, 
    bitraverseDefault: bitraverseDefault, 
    bisequence: bisequence, 
    bitraverse: bitraverse
};

},{"../Data.Bifoldable":103,"../Data.Bifunctor":104,"../Prelude":203}],106:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Tuple = require("../Data.Tuple");
var Data_CatQueue = require("../Data.CatQueue");
var Data_List = require("../Data.List");
var CatNil = (function () {
    function CatNil() {

    };
    CatNil.value = new CatNil();
    return CatNil;
})();
var CatCons = (function () {
    function CatCons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    CatCons.create = function (value0) {
        return function (value1) {
            return new CatCons(value0, value1);
        };
    };
    return CatCons;
})();
var showCatList = function (dictShow) {
    return new Prelude.Show(function (v) {
        if (v instanceof CatNil) {
            return "CatNil";
        };
        if (v instanceof CatCons) {
            return "CatList (" + (Prelude.show(dictShow)(v.value0) + (") (" + (Prelude.show(Data_CatQueue.showCatQueue(showCatList(dictShow)))(v.value1) + ")")));
        };
        throw new Error("Failed pattern match at Data.CatList line 114, column 3 - line 115, column 3: " + [ v.constructor.name ]);
    });
};
var $$null = function (v) {
    if (v instanceof CatNil) {
        return true;
    };
    return false;
};
var link = function (v) {
    return function (cat) {
        if (v instanceof CatNil) {
            return cat;
        };
        if (v instanceof CatCons) {
            return new CatCons(v.value0, Data_CatQueue.snoc(v.value1)(cat));
        };
        throw new Error("Failed pattern match at Data.CatList line 89, column 1 - line 90, column 1: " + [ v.constructor.name, cat.constructor.name ]);
    };
};
var foldr = function (k) {
    return function (b) {
        return function (q) {
            var foldl = function (__copy_v) {
                return function (__copy_c) {
                    return function (__copy_v1) {
                        var v = __copy_v;
                        var c = __copy_c;
                        var v1 = __copy_v1;
                        tco: while (true) {
                            if (v1 instanceof Data_List.Nil) {
                                return c;
                            };
                            if (v1 instanceof Data_List.Cons) {
                                var __tco_v = v;
                                var __tco_c = v(c)(v1.value0);
                                var __tco_v1 = v1.value1;
                                v = __tco_v;
                                c = __tco_c;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            throw new Error("Failed pattern match at Data.CatList line 104, column 3 - line 105, column 3: " + [ v.constructor.name, c.constructor.name, v1.constructor.name ]);
                        };
                    };
                };
            };
            var go = function (__copy_xs) {
                return function (__copy_ys) {
                    var xs = __copy_xs;
                    var ys = __copy_ys;
                    tco: while (true) {
                        var $22 = Data_CatQueue.uncons(xs);
                        if ($22 instanceof Data_Maybe.Nothing) {
                            return foldl(function (x) {
                                return function (i) {
                                    return i(x);
                                };
                            })(b)(ys);
                        };
                        if ($22 instanceof Data_Maybe.Just) {
                            var __tco_ys = new Data_List.Cons(k($22.value0.value0), ys);
                            xs = $22.value0.value1;
                            ys = __tco_ys;
                            continue tco;
                        };
                        throw new Error("Failed pattern match at Data.CatList line 99, column 14 - line 103, column 3: " + [ $22.constructor.name ]);
                    };
                };
            };
            return go(q)(Data_List.Nil.value);
        };
    };
};
var uncons = function (v) {
    if (v instanceof CatNil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof CatCons) {
        return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, (function () {
            var $27 = Data_CatQueue["null"](v.value1);
            if ($27) {
                return CatNil.value;
            };
            if (!$27) {
                return foldr(link)(CatNil.value)(v.value1);
            };
            throw new Error("Failed pattern match at Data.CatList line 81, column 39 - line 81, column 89: " + [ $27.constructor.name ]);
        })()));
    };
    throw new Error("Failed pattern match at Data.CatList line 80, column 1 - line 81, column 1: " + [ v.constructor.name ]);
};
var empty = CatNil.value;
var append = function (v) {
    return function (v1) {
        if (v1 instanceof CatNil) {
            return v;
        };
        if (v instanceof CatNil) {
            return v1;
        };
        return link(v)(v1);
    };
};
var cons = function (a) {
    return function (cat) {
        return append(new CatCons(a, Data_CatQueue.empty))(cat);
    };
};
var semigroupCatList = new Prelude.Semigroup(append);
var monoidCatList = new Data_Monoid.Monoid(function () {
    return semigroupCatList;
}, CatNil.value);
var snoc = function (cat) {
    return function (a) {
        return append(cat)(new CatCons(a, Data_CatQueue.empty));
    };
};
module.exports = {
    CatNil: CatNil, 
    CatCons: CatCons, 
    uncons: uncons, 
    snoc: snoc, 
    cons: cons, 
    append: append, 
    "null": $$null, 
    empty: empty, 
    semigroupCatList: semigroupCatList, 
    monoidCatList: monoidCatList, 
    showCatList: showCatList
};

},{"../Data.CatQueue":107,"../Data.List":146,"../Data.Maybe":152,"../Data.Monoid":159,"../Data.Tuple":171,"../Prelude":203}],107:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_List = require("../Data.List");
var Data_Maybe = require("../Data.Maybe");
var Data_Tuple = require("../Data.Tuple");
var CatQueue = (function () {
    function CatQueue(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    CatQueue.create = function (value0) {
        return function (value1) {
            return new CatQueue(value0, value1);
        };
    };
    return CatQueue;
})();
var uncons = function (__copy_v) {
    var v = __copy_v;
    tco: while (true) {
        if (v.value0 instanceof Data_List.Nil && v.value1 instanceof Data_List.Nil) {
            return Data_Maybe.Nothing.value;
        };
        if (v.value0 instanceof Data_List.Nil) {
            var __tco_v = new CatQueue(Data_List.reverse(v.value1), Data_List.Nil.value);
            v = __tco_v;
            continue tco;
        };
        if (v.value0 instanceof Data_List.Cons) {
            return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
        };
        throw new Error("Failed pattern match at Data.CatQueue line 51, column 1 - line 52, column 1: " + [ v.constructor.name ]);
    };
};
var snoc = function (v) {
    return function (a) {
        return new CatQueue(v.value0, new Data_List.Cons(a, v.value1));
    };
};
var showCatQueue = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "CatQueue (" + (Prelude.show(Data_List.showList(dictShow))(v.value0) + (") (" + (Prelude.show(Data_List.showList(dictShow))(v.value1) + ")")));
    });
};
var $$null = function (v) {
    if (v.value0 instanceof Data_List.Nil && v.value1 instanceof Data_List.Nil) {
        return true;
    };
    return false;
};
var empty = new CatQueue(Data_List.Nil.value, Data_List.Nil.value);
module.exports = {
    CatQueue: CatQueue, 
    uncons: uncons, 
    snoc: snoc, 
    "null": $$null, 
    empty: empty, 
    showCatQueue: showCatQueue
};

},{"../Data.List":146,"../Data.Maybe":152,"../Data.Tuple":171,"../Prelude":203}],108:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Char

exports.toString = function (c) {
  return c;
};

exports.toCharCode = function (c) {
  return c.charCodeAt(0);
};

exports.fromCharCode = function (c) {
  return String.fromCharCode(c);
};

exports.toLower = function (c) {
  return c.toLowerCase();
};

exports.toUpper = function (c) {
  return c.toUpperCase();
};

},{}],109:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
module.exports = {
    toUpper: $foreign.toUpper, 
    toLower: $foreign.toLower, 
    toCharCode: $foreign.toCharCode, 
    fromCharCode: $foreign.fromCharCode, 
    toString: $foreign.toString
};

},{"../Prelude":203,"./foreign":108}],110:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Bifoldable = require("../Data.Bifoldable");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor_Contravariant = require("../Data.Functor.Contravariant");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Traversable = require("../Data.Traversable");
var Const = function (x) {
    return x;
};
var showConst = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Const (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semigroupoidConst = new Prelude.Semigroupoid(function (v) {
    return function (v1) {
        return v1;
    };
});
var semigroupConst = function (dictSemigroup) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude["<>"](dictSemigroup)(v)(v1);
        };
    });
};
var monoidConst = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupConst(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Data_Monoid.mempty(dictMonoid));
};
var getConst = function (v) {
    return v;
};
var functorConst = new Prelude.Functor(function (v) {
    return function (v1) {
        return v1;
    };
});
var invariantConst = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorConst));
var foldableConst = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (v) {
        return function (v1) {
            return Data_Monoid.mempty(dictMonoid);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            return z;
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            return z;
        };
    };
});
var traversableConst = new Data_Traversable.Traversable(function () {
    return foldableConst;
}, function () {
    return functorConst;
}, function (dictApplicative) {
    return function (v) {
        return Prelude.pure(dictApplicative)(v);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            return Prelude.pure(dictApplicative)(v1);
        };
    };
});
var eqConst = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordConst = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqConst(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var contravariantConst = new Data_Functor_Contravariant.Contravariant(function (v) {
    return function (v1) {
        return v1;
    };
});
var boundedConst = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(dictBounded), Prelude.top(dictBounded));
};
var applyConst = function (dictSemigroup) {
    return new Prelude.Apply(function () {
        return functorConst;
    }, function (v) {
        return function (v1) {
            return Prelude["<>"](dictSemigroup)(v)(v1);
        };
    });
};
var bindConst = function (dictSemigroup) {
    return new Prelude.Bind(function () {
        return applyConst(dictSemigroup);
    }, function (v) {
        return function (v1) {
            return v;
        };
    });
};
var applicativeConst = function (dictMonoid) {
    return new Prelude.Applicative(function () {
        return applyConst(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, function (v) {
        return Data_Monoid.mempty(dictMonoid);
    });
};
module.exports = {
    Const: Const, 
    getConst: getConst, 
    eqConst: eqConst, 
    ordConst: ordConst, 
    boundedConst: boundedConst, 
    showConst: showConst, 
    semigroupoidConst: semigroupoidConst, 
    semigroupConst: semigroupConst, 
    monoidConst: monoidConst, 
    functorConst: functorConst, 
    invariantConst: invariantConst, 
    applyConst: applyConst, 
    bindConst: bindConst, 
    applicativeConst: applicativeConst, 
    contravariantConst: contravariantConst, 
    foldableConst: foldableConst, 
    traversableConst: traversableConst
};

},{"../Data.Bifoldable":103,"../Data.Foldable":120,"../Data.Functor.Contravariant":131,"../Data.Functor.Invariant":133,"../Data.Monoid":159,"../Data.Traversable":170,"../Prelude":203}],111:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Identity = require("../Data.Identity");
var Distributive = function (__superclass_Prelude$dotFunctor_0, collect, distribute) {
    this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    this.collect = collect;
    this.distribute = distribute;
};
var distributiveIdentity = new Distributive(function () {
    return Data_Identity.functorIdentity;
}, function (dictFunctor) {
    return function (f) {
        return function ($7) {
            return Data_Identity.Identity(Prelude.map(dictFunctor)(function ($8) {
                return Data_Identity.runIdentity(f($8));
            })($7));
        };
    };
}, function (dictFunctor) {
    return function ($9) {
        return Data_Identity.Identity(Prelude.map(dictFunctor)(Data_Identity.runIdentity)($9));
    };
});
var distribute = function (dict) {
    return dict.distribute;
};
var distributiveFunction = new Distributive(function () {
    return Prelude.functorFn;
}, function (dictFunctor) {
    return function (f) {
        return function ($10) {
            return distribute(distributiveFunction)(dictFunctor)(Prelude.map(dictFunctor)(f)($10));
        };
    };
}, function (dictFunctor) {
    return function (a) {
        return function (e) {
            return Prelude.map(dictFunctor)(function (v) {
                return v(e);
            })(a);
        };
    };
});
var cotraverse = function (dictDistributive) {
    return function (dictFunctor) {
        return function (f) {
            return function ($11) {
                return Prelude.map(dictDistributive["__superclass_Prelude.Functor_0"]())(f)(distribute(dictDistributive)(dictFunctor)($11));
            };
        };
    };
};
var collect = function (dict) {
    return dict.collect;
};
module.exports = {
    Distributive: Distributive, 
    cotraverse: cotraverse, 
    collect: collect, 
    distribute: distribute, 
    distributiveIdentity: distributiveIdentity, 
    distributiveFunction: distributiveFunction
};

},{"../Data.Identity":137,"../Prelude":203}],112:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Either.Unsafe

exports.unsafeThrow = function (msg) {
  throw new Error(msg);
};

},{}],113:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var fromRight = function (v) {
    if (v instanceof Data_Either.Right) {
        return v.value0;
    };
    return $foreign.unsafeThrow("Data.Either.Unsafe.fromRight called on Left value");
};
var fromLeft = function (v) {
    if (v instanceof Data_Either.Left) {
        return v.value0;
    };
    return $foreign.unsafeThrow("Data.Either.Unsafe.fromLeft called on Right value");
};
module.exports = {
    fromRight: fromRight, 
    fromLeft: fromLeft
};

},{"../Data.Either":114,"../Prelude":203,"./foreign":112}],114:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Extend = require("../Control.Extend");
var Data_Bifoldable = require("../Data.Bifoldable");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Bitraversable = require("../Data.Bitraversable");
var Data_Foldable = require("../Data.Foldable");
var Data_Monoid = require("../Data.Monoid");
var Data_Traversable = require("../Data.Traversable");
var Left = (function () {
    function Left(value0) {
        this.value0 = value0;
    };
    Left.create = function (value0) {
        return new Left(value0);
    };
    return Left;
})();
var Right = (function () {
    function Right(value0) {
        this.value0 = value0;
    };
    Right.create = function (value0) {
        return new Right(value0);
    };
    return Right;
})();
var showEither = function (dictShow) {
    return function (dictShow1) {
        return new Prelude.Show(function (v) {
            if (v instanceof Left) {
                return "Left (" + (Prelude.show(dictShow)(v.value0) + ")");
            };
            if (v instanceof Right) {
                return "Right (" + (Prelude.show(dictShow1)(v.value0) + ")");
            };
            throw new Error("Failed pattern match at Data.Either line 175, column 3 - line 176, column 3: " + [ v.constructor.name ]);
        });
    };
};
var functorEither = new Prelude.Functor(function (v) {
    return function (v1) {
        if (v1 instanceof Left) {
            return new Left(v1.value0);
        };
        if (v1 instanceof Right) {
            return new Right(v(v1.value0));
        };
        throw new Error("Failed pattern match at Data.Either line 53, column 3 - line 54, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var foldableEither = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof Left) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof Right) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 206, column 3 - line 207, column 3: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Left) {
                return z;
            };
            if (v1 instanceof Right) {
                return v(z)(v1.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 204, column 3 - line 205, column 3: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Left) {
                return z;
            };
            if (v1 instanceof Right) {
                return v(v1.value0)(z);
            };
            throw new Error("Failed pattern match at Data.Either line 202, column 3 - line 203, column 3: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
});
var traversableEither = new Data_Traversable.Traversable(function () {
    return foldableEither;
}, function () {
    return functorEither;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Left) {
            return Prelude.pure(dictApplicative)(new Left(v.value0));
        };
        if (v instanceof Right) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Right.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Either line 220, column 3 - line 221, column 3: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Left) {
                return Prelude.pure(dictApplicative)(new Left(v1.value0));
            };
            if (v1 instanceof Right) {
                return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Right.create)(v(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Either line 218, column 3 - line 219, column 3: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var extendEither = new Control_Extend.Extend(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v1 instanceof Left) {
            return new Left(v1.value0);
        };
        return new Right(v(v1));
    };
});
var eqEither = function (dictEq) {
    return function (dictEq1) {
        return new Prelude.Eq(function (v) {
            return function (v1) {
                if (v instanceof Left && v1 instanceof Left) {
                    return Prelude["=="](dictEq)(v.value0)(v1.value0);
                };
                if (v instanceof Right && v1 instanceof Right) {
                    return Prelude["=="](dictEq1)(v.value0)(v1.value0);
                };
                return false;
            };
        });
    };
};
var ordEither = function (dictOrd) {
    return function (dictOrd1) {
        return new Prelude.Ord(function () {
            return eqEither(dictOrd["__superclass_Prelude.Eq_0"]())(dictOrd1["__superclass_Prelude.Eq_0"]());
        }, function (v) {
            return function (v1) {
                if (v instanceof Left && v1 instanceof Left) {
                    return Prelude.compare(dictOrd)(v.value0)(v1.value0);
                };
                if (v instanceof Right && v1 instanceof Right) {
                    return Prelude.compare(dictOrd1)(v.value0)(v1.value0);
                };
                if (v instanceof Left) {
                    return Prelude.LT.value;
                };
                if (v1 instanceof Left) {
                    return Prelude.GT.value;
                };
                throw new Error("Failed pattern match at Data.Either line 192, column 3 - line 193, column 3: " + [ v.constructor.name, v1.constructor.name ]);
            };
        });
    };
};
var either = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Left) {
                return v(v2.value0);
            };
            if (v2 instanceof Right) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 29, column 1 - line 30, column 1: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var isLeft = either(Prelude["const"](true))(Prelude["const"](false));
var isRight = either(Prelude["const"](false))(Prelude["const"](true));
var boundedEither = function (dictBounded) {
    return function (dictBounded1) {
        return new Prelude.Bounded(new Left(Prelude.bottom(dictBounded)), new Right(Prelude.top(dictBounded1)));
    };
};
var bifunctorEither = new Data_Bifunctor.Bifunctor(function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Left) {
                return new Left(v(v2.value0));
            };
            if (v2 instanceof Right) {
                return new Right(v1(v2.value0));
            };
            throw new Error("Failed pattern match at Data.Either line 57, column 3 - line 58, column 3: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
});
var bifoldableEither = new Data_Bifoldable.Bifoldable(function (dictMonoid) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(v2.value0);
                };
                if (v2 instanceof Right) {
                    return v1(v2.value0);
                };
                throw new Error("Failed pattern match at Data.Either line 214, column 3 - line 215, column 3: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    };
}, function (v) {
    return function (v1) {
        return function (z) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(z)(v2.value0);
                };
                if (v2 instanceof Right) {
                    return v1(z)(v2.value0);
                };
                throw new Error("Failed pattern match at Data.Either line 212, column 3 - line 213, column 3: " + [ v.constructor.name, v1.constructor.name, z.constructor.name, v2.constructor.name ]);
            };
        };
    };
}, function (v) {
    return function (v1) {
        return function (z) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(v2.value0)(z);
                };
                if (v2 instanceof Right) {
                    return v1(v2.value0)(z);
                };
                throw new Error("Failed pattern match at Data.Either line 210, column 3 - line 211, column 3: " + [ v.constructor.name, v1.constructor.name, z.constructor.name, v2.constructor.name ]);
            };
        };
    };
});
var bitraversableEither = new Data_Bitraversable.Bitraversable(function () {
    return bifoldableEither;
}, function () {
    return bifunctorEither;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Left) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Left.create)(v.value0);
        };
        if (v instanceof Right) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Right.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Either line 226, column 3 - line 227, column 3: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Left.create)(v(v2.value0));
                };
                if (v2 instanceof Right) {
                    return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Right.create)(v1(v2.value0));
                };
                throw new Error("Failed pattern match at Data.Either line 224, column 3 - line 225, column 3: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    };
});
var applyEither = new Prelude.Apply(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Left) {
            return new Left(v.value0);
        };
        if (v instanceof Right) {
            return Prelude["<$>"](functorEither)(v.value0)(v1);
        };
        throw new Error("Failed pattern match at Data.Either line 93, column 3 - line 94, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindEither = new Prelude.Bind(function () {
    return applyEither;
}, either(function (e) {
    return function (v) {
        return new Left(e);
    };
})(function (a) {
    return function (f) {
        return f(a);
    };
}));
var semigroupEither = function (dictSemigroup) {
    return new Prelude.Semigroup(function (x) {
        return function (y) {
            return Prelude["<*>"](applyEither)(Prelude["<$>"](functorEither)(Prelude.append(dictSemigroup))(x))(y);
        };
    });
};
var semiringEither = function (dictSemiring) {
    return new Prelude.Semiring(function (x) {
        return function (y) {
            return Prelude["<*>"](applyEither)(Prelude["<$>"](functorEither)(Prelude.add(dictSemiring))(x))(y);
        };
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyEither)(Prelude["<$>"](functorEither)(Prelude.mul(dictSemiring))(x))(y);
        };
    }, new Right(Prelude.one(dictSemiring)), new Right(Prelude.zero(dictSemiring)));
};
var applicativeEither = new Prelude.Applicative(function () {
    return applyEither;
}, Right.create);
var monadEither = new Prelude.Monad(function () {
    return applicativeEither;
}, function () {
    return bindEither;
});
var altEither = new Control_Alt.Alt(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Left) {
            return v1;
        };
        return v;
    };
});
module.exports = {
    Left: Left, 
    Right: Right, 
    isRight: isRight, 
    isLeft: isLeft, 
    either: either, 
    functorEither: functorEither, 
    bifunctorEither: bifunctorEither, 
    applyEither: applyEither, 
    applicativeEither: applicativeEither, 
    altEither: altEither, 
    bindEither: bindEither, 
    monadEither: monadEither, 
    extendEither: extendEither, 
    showEither: showEither, 
    eqEither: eqEither, 
    ordEither: ordEither, 
    boundedEither: boundedEither, 
    foldableEither: foldableEither, 
    bifoldableEither: bifoldableEither, 
    traversableEither: traversableEither, 
    bitraversableEither: bitraversableEither, 
    semiringEither: semiringEither, 
    semigroupEither: semigroupEither
};

},{"../Control.Alt":26,"../Control.Extend":36,"../Data.Bifoldable":103,"../Data.Bifunctor":104,"../Data.Bitraversable":105,"../Data.Foldable":120,"../Data.Monoid":159,"../Data.Traversable":170,"../Prelude":203}],115:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Char = require("../Data.Char");
var Data_Either = require("../Data.Either");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_Unsafe = require("../Data.Maybe.Unsafe");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Cardinality = function (x) {
    return x;
};
var Enum = function (__superclass_Prelude$dotBounded_0, cardinality, fromEnum, pred, succ, toEnum) {
    this["__superclass_Prelude.Bounded_0"] = __superclass_Prelude$dotBounded_0;
    this.cardinality = cardinality;
    this.fromEnum = fromEnum;
    this.pred = pred;
    this.succ = succ;
    this.toEnum = toEnum;
};
var toEnum = function (dict) {
    return dict.toEnum;
};
var succ = function (dict) {
    return dict.succ;
};
var runCardinality = function (v) {
    return v;
};
var tupleCardinality = function (dictEnum) {
    return function (dictEnum1) {
        return function (l) {
            return function (r) {
                return Cardinality(runCardinality(l) * runCardinality(r) | 0);
            };
        };
    };
};
var tupleToEnum = function (dictEnum) {
    return function (dictEnum1) {
        return function (cardb) {
            return function (n) {
                return Prelude["<*>"](Data_Maybe.applyMaybe)(Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Tuple.Tuple.create)(toEnum(dictEnum)(n / runCardinality(cardb) | 0)))(toEnum(dictEnum1)(n % runCardinality(cardb)));
            };
        };
    };
};
var pred = function (dict) {
    return dict.pred;
};
var maybeCardinality = function (dictEnum) {
    return function (c) {
        return Cardinality(1 + runCardinality(c) | 0);
    };
};
var maybeToEnum = function (dictEnum) {
    return function (v) {
        return function (v1) {
            if (v1 <= runCardinality(maybeCardinality(dictEnum)(v))) {
                var $39 = v1 === 0;
                if ($39) {
                    return Data_Maybe.Just.create(Data_Maybe.Nothing.value);
                };
                if (!$39) {
                    return Data_Maybe.Just.create(toEnum(dictEnum)(v1 - 1));
                };
                throw new Error("Failed pattern match at Data.Enum line 140, column 3 - line 143, column 1: " + [ $39.constructor.name ]);
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var intStepFromTo = function (step) {
    return function (from) {
        return function (to) {
            return Data_Unfoldable.unfoldr(Data_Unfoldable.unfoldableArray)(function (e) {
                var $40 = e <= to;
                if ($40) {
                    return Data_Maybe.Just.create(new Data_Tuple.Tuple(e, e + step | 0));
                };
                if (!$40) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.Enum line 106, column 13 - line 109, column 11: " + [ $40.constructor.name ]);
            })(from);
        };
    };
};
var intFromTo = intStepFromTo(1);
var fromEnum = function (dict) {
    return dict.fromEnum;
};
var tupleFromEnum = function (dictEnum) {
    return function (dictEnum1) {
        return function (cardb) {
            return function (v) {
                return (fromEnum(dictEnum)(v.value0) * runCardinality(cardb) | 0) + fromEnum(dictEnum1)(v.value1) | 0;
            };
        };
    };
};
var enumFromTo = function (dictEnum) {
    return function (a) {
        return function (b) {
            var b$prime = fromEnum(dictEnum)(b);
            var a$prime = fromEnum(dictEnum)(a);
            return Prelude["<$>"](Prelude.functorArray)(function ($75) {
                return Data_Maybe_Unsafe.fromJust(toEnum(dictEnum)($75));
            })(intFromTo(a$prime)(b$prime));
        };
    };
};
var enumFromThenTo = function (dictEnum) {
    return function (a) {
        return function (b) {
            return function (c) {
                var c$prime = fromEnum(dictEnum)(c);
                var b$prime = fromEnum(dictEnum)(b);
                var a$prime = fromEnum(dictEnum)(a);
                return Prelude["<$>"](Prelude.functorArray)(function ($76) {
                    return Data_Maybe_Unsafe.fromJust(toEnum(dictEnum)($76));
                })(intStepFromTo(b$prime - a$prime)(a$prime)(c$prime));
            };
        };
    };
};
var eitherFromEnum = function (dictEnum) {
    return function (dictEnum1) {
        return function (carda) {
            return function (v) {
                if (v instanceof Data_Either.Left) {
                    return fromEnum(dictEnum)(v.value0);
                };
                if (v instanceof Data_Either.Right) {
                    return fromEnum(dictEnum1)(v.value0) + runCardinality(carda) | 0;
                };
                throw new Error("Failed pattern match at Data.Enum line 198, column 1 - line 199, column 1: " + [ carda.constructor.name, v.constructor.name ]);
            };
        };
    };
};
var eitherCardinality = function (dictEnum) {
    return function (dictEnum1) {
        return function (l) {
            return function (r) {
                return Cardinality(runCardinality(l) + runCardinality(r) | 0);
            };
        };
    };
};
var eitherToEnum = function (dictEnum) {
    return function (dictEnum1) {
        return function (carda) {
            return function (cardb) {
                return function (n) {
                    var $49 = n >= 0 && n < runCardinality(carda);
                    if ($49) {
                        return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Either.Left.create)(toEnum(dictEnum)(n));
                    };
                    if (!$49) {
                        var $50 = n >= runCardinality(carda) && n < runCardinality(eitherCardinality(dictEnum)(dictEnum1)(carda)(cardb));
                        if ($50) {
                            return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Either.Right.create)(toEnum(dictEnum1)(n - runCardinality(carda)));
                        };
                        if (!$50) {
                            return Data_Maybe.Nothing.value;
                        };
                        throw new Error("Failed pattern match at Data.Enum line 193, column 12 - line 197, column 1: " + [ $50.constructor.name ]);
                    };
                    throw new Error("Failed pattern match at Data.Enum line 191, column 7 - line 197, column 1: " + [ $49.constructor.name ]);
                };
            };
        };
    };
};
var defaultToEnum = function (succ$prime) {
    return function (bottom$prime) {
        return function (n) {
            if (n < 0) {
                return Data_Maybe.Nothing.value;
            };
            if (n === 0) {
                return new Data_Maybe.Just(bottom$prime);
            };
            if (Prelude.otherwise) {
                return Prelude[">>="](Data_Maybe.bindMaybe)(defaultToEnum(succ$prime)(bottom$prime)(n - 1))(succ$prime);
            };
            throw new Error("Failed pattern match at Data.Enum line 71, column 1 - line 78, column 1: " + [ succ$prime.constructor.name, bottom$prime.constructor.name, n.constructor.name ]);
        };
    };
};
var defaultSucc = function (toEnum$prime) {
    return function (fromEnum$prime) {
        return function (a) {
            return toEnum$prime(fromEnum$prime(a) + 1 | 0);
        };
    };
};
var defaultPred = function (toEnum$prime) {
    return function (fromEnum$prime) {
        return function (a) {
            return toEnum$prime(fromEnum$prime(a) - 1);
        };
    };
};
var defaultFromEnum = function (pred$prime) {
    return function (e) {
        return Data_Maybe.maybe(0)(function (prd) {
            return defaultFromEnum(pred$prime)(prd) + 1 | 0;
        })(pred$prime(e));
    };
};
var charToEnum = function (v) {
    if (v >= 0 && v <= 65535) {
        return Data_Maybe.Just.create(Data_Char.fromCharCode(v));
    };
    return Data_Maybe.Nothing.value;
};
var charFromEnum = Data_Char.toCharCode;
var enumChar = new Enum(function () {
    return Prelude.boundedChar;
}, 65536, charFromEnum, defaultPred(charToEnum)(charFromEnum), defaultSucc(charToEnum)(charFromEnum), charToEnum);
var cardinality = function (dict) {
    return dict.cardinality;
};
var enumEither = function (dictEnum) {
    return function (dictEnum1) {
        return new Enum(function () {
            return Data_Either.boundedEither(dictEnum["__superclass_Prelude.Bounded_0"]())(dictEnum1["__superclass_Prelude.Bounded_0"]());
        }, eitherCardinality(dictEnum)(dictEnum1)(cardinality(dictEnum))(cardinality(dictEnum1)), eitherFromEnum(dictEnum)(dictEnum1)(cardinality(dictEnum)), function (v) {
            if (v instanceof Data_Either.Left) {
                return Data_Maybe.maybe(Data_Maybe.Nothing.value)(function ($77) {
                    return Data_Maybe.Just.create(Data_Either.Left.create($77));
                })(pred(dictEnum)(v.value0));
            };
            if (v instanceof Data_Either.Right) {
                return Data_Maybe.maybe(Data_Maybe.Just.create(new Data_Either.Left(Prelude.top(dictEnum["__superclass_Prelude.Bounded_0"]()))))(function ($78) {
                    return Data_Maybe.Just.create(Data_Either.Right.create($78));
                })(pred(dictEnum1)(v.value0));
            };
            throw new Error("Failed pattern match at Data.Enum line 184, column 3 - line 185, column 3: " + [ v.constructor.name ]);
        }, function (v) {
            if (v instanceof Data_Either.Left) {
                return Data_Maybe.maybe(Data_Maybe.Just.create(new Data_Either.Right(Prelude.bottom(dictEnum1["__superclass_Prelude.Bounded_0"]()))))(function ($79) {
                    return Data_Maybe.Just.create(Data_Either.Left.create($79));
                })(succ(dictEnum)(v.value0));
            };
            if (v instanceof Data_Either.Right) {
                return Data_Maybe.maybe(Data_Maybe.Nothing.value)(function ($80) {
                    return Data_Maybe.Just.create(Data_Either.Right.create($80));
                })(succ(dictEnum1)(v.value0));
            };
            throw new Error("Failed pattern match at Data.Enum line 182, column 3 - line 183, column 3: " + [ v.constructor.name ]);
        }, eitherToEnum(dictEnum)(dictEnum1)(cardinality(dictEnum))(cardinality(dictEnum1)));
    };
};
var enumMaybe = function (dictEnum) {
    return new Enum(function () {
        return Data_Maybe.boundedMaybe(dictEnum["__superclass_Prelude.Bounded_0"]());
    }, maybeCardinality(dictEnum)(cardinality(dictEnum)), function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return 0;
        };
        if (v instanceof Data_Maybe.Just) {
            return fromEnum(dictEnum)(v.value0) + 1 | 0;
        };
        throw new Error("Failed pattern match at Data.Enum line 135, column 3 - line 136, column 3: " + [ v.constructor.name ]);
    }, function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        if (v instanceof Data_Maybe.Just) {
            return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Maybe.Just.create)(pred(dictEnum)(v.value0));
        };
        throw new Error("Failed pattern match at Data.Enum line 132, column 3 - line 133, column 3: " + [ v.constructor.name ]);
    }, function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Just.create(Prelude.bottom(Data_Maybe.boundedMaybe(dictEnum["__superclass_Prelude.Bounded_0"]())));
        };
        if (v instanceof Data_Maybe.Just) {
            return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Maybe.Just.create)(succ(dictEnum)(v.value0));
        };
        throw new Error("Failed pattern match at Data.Enum line 130, column 3 - line 131, column 3: " + [ v.constructor.name ]);
    }, maybeToEnum(dictEnum)(cardinality(dictEnum)));
};
var enumTuple = function (dictEnum) {
    return function (dictEnum1) {
        return new Enum(function () {
            return Data_Tuple.boundedTuple(dictEnum["__superclass_Prelude.Bounded_0"]())(dictEnum1["__superclass_Prelude.Bounded_0"]());
        }, tupleCardinality(dictEnum)(dictEnum1)(cardinality(dictEnum))(cardinality(dictEnum1)), tupleFromEnum(dictEnum)(dictEnum1)(cardinality(dictEnum1)), function (v) {
            return Data_Maybe.maybe(Prelude["<$>"](Data_Maybe.functorMaybe)(Prelude.flip(Data_Tuple.Tuple.create)(Prelude.bottom(dictEnum1["__superclass_Prelude.Bounded_0"]())))(pred(dictEnum)(v.value0)))(function ($81) {
                return Data_Maybe.Just.create(Data_Tuple.Tuple.create(v.value0)($81));
            })(pred(dictEnum1)(v.value1));
        }, function (v) {
            return Data_Maybe.maybe(Prelude["<$>"](Data_Maybe.functorMaybe)(Prelude.flip(Data_Tuple.Tuple.create)(Prelude.bottom(dictEnum1["__superclass_Prelude.Bounded_0"]())))(succ(dictEnum)(v.value0)))(function ($82) {
                return Data_Maybe.Just.create(Data_Tuple.Tuple.create(v.value0)($82));
            })(succ(dictEnum1)(v.value1));
        }, tupleToEnum(dictEnum)(dictEnum1)(cardinality(dictEnum1)));
    };
};
var booleanSucc = function (v) {
    if (!v) {
        return new Data_Maybe.Just(true);
    };
    return Data_Maybe.Nothing.value;
};
var booleanPred = function (v) {
    if (v) {
        return new Data_Maybe.Just(false);
    };
    return Data_Maybe.Nothing.value;
};
var enumBoolean = new Enum(function () {
    return Prelude.boundedBoolean;
}, 2, defaultFromEnum(booleanPred), booleanPred, booleanSucc, defaultToEnum(booleanSucc)(false));
module.exports = {
    Cardinality: Cardinality, 
    Enum: Enum, 
    enumFromThenTo: enumFromThenTo, 
    enumFromTo: enumFromTo, 
    intStepFromTo: intStepFromTo, 
    intFromTo: intFromTo, 
    defaultFromEnum: defaultFromEnum, 
    defaultToEnum: defaultToEnum, 
    defaultPred: defaultPred, 
    defaultSucc: defaultSucc, 
    toEnum: toEnum, 
    succ: succ, 
    runCardinality: runCardinality, 
    pred: pred, 
    fromEnum: fromEnum, 
    cardinality: cardinality, 
    enumChar: enumChar, 
    enumMaybe: enumMaybe, 
    enumBoolean: enumBoolean, 
    enumTuple: enumTuple, 
    enumEither: enumEither
};

},{"../Data.Char":109,"../Data.Either":114,"../Data.Maybe":152,"../Data.Maybe.Unsafe":151,"../Data.Tuple":171,"../Data.Unfoldable":172,"../Prelude":203}],116:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Exists

exports.mkExists = function (fa) {
  return fa;
};

exports.runExists = function (f) {
  return function (fa) {
    return f(fa);
  };
};

},{}],117:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
module.exports = {
    runExists: $foreign.runExists, 
    mkExists: $foreign.mkExists
};

},{"../Prelude":203,"./foreign":116}],118:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Unsafe_Coerce = require("../Unsafe.Coerce");
var runExistsR = Unsafe_Coerce.unsafeCoerce;
var mkExistsR = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    runExistsR: runExistsR, 
    mkExistsR: mkExistsR
};

},{"../Unsafe.Coerce":206}],119:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Foldable

exports.foldrArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};

exports.foldlArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

},{}],120:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Apply = require("../Control.Apply");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_First = require("../Data.Maybe.First");
var Data_Maybe_Last = require("../Data.Maybe.Last");
var Data_Monoid = require("../Data.Monoid");
var Data_Monoid_Additive = require("../Data.Monoid.Additive");
var Data_Monoid_Conj = require("../Data.Monoid.Conj");
var Data_Monoid_Disj = require("../Data.Monoid.Disj");
var Data_Monoid_Dual = require("../Data.Monoid.Dual");
var Data_Monoid_Endo = require("../Data.Monoid.Endo");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative");
var Foldable = function (foldMap, foldl, foldr) {
    this.foldMap = foldMap;
    this.foldl = foldl;
    this.foldr = foldr;
};
var foldr = function (dict) {
    return dict.foldr;
};
var traverse_ = function (dictApplicative) {
    return function (dictFoldable) {
        return function (f) {
            return foldr(dictFoldable)(function ($161) {
                return Control_Apply["*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(f($161));
            })(Prelude.pure(dictApplicative)(Prelude.unit));
        };
    };
};
var for_ = function (dictApplicative) {
    return function (dictFoldable) {
        return Prelude.flip(traverse_(dictApplicative)(dictFoldable));
    };
};
var sequence_ = function (dictApplicative) {
    return function (dictFoldable) {
        return traverse_(dictApplicative)(dictFoldable)(Prelude.id(Prelude.categoryFn));
    };
};
var foldl = function (dict) {
    return dict.foldl;
};
var intercalate = function (dictFoldable) {
    return function (dictMonoid) {
        return function (sep) {
            return function (xs) {
                var go = function (v) {
                    return function (x) {
                        if (v.init) {
                            return {
                                init: false, 
                                acc: x
                            };
                        };
                        return {
                            init: false, 
                            acc: Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.acc)(Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(sep)(x))
                        };
                    };
                };
                return (foldl(dictFoldable)(go)({
                    init: true, 
                    acc: Data_Monoid.mempty(dictMonoid)
                })(xs)).acc;
            };
        };
    };
};
var maximumBy = function (dictFoldable) {
    return function (cmp) {
        var max$prime = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Maybe.Just(v1);
                };
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just((function () {
                        var $87 = cmp(v.value0)(v1);
                        if ($87 instanceof Prelude.GT) {
                            return v.value0;
                        };
                        return v1;
                    })());
                };
                throw new Error("Failed pattern match at Data.Foldable line 246, column 3 - line 247, column 3: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return foldl(dictFoldable)(max$prime)(Data_Maybe.Nothing.value);
    };
};
var maximum = function (dictOrd) {
    return function (dictFoldable) {
        return maximumBy(dictFoldable)(Prelude.compare(dictOrd));
    };
};
var mconcat = function (dictFoldable) {
    return function (dictMonoid) {
        return foldl(dictFoldable)(Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]()))(Data_Monoid.mempty(dictMonoid));
    };
};
var minimumBy = function (dictFoldable) {
    return function (cmp) {
        var min$prime = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Maybe.Just(v1);
                };
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just((function () {
                        var $91 = cmp(v.value0)(v1);
                        if ($91 instanceof Prelude.LT) {
                            return v.value0;
                        };
                        return v1;
                    })());
                };
                throw new Error("Failed pattern match at Data.Foldable line 261, column 3 - line 262, column 3: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return foldl(dictFoldable)(min$prime)(Data_Maybe.Nothing.value);
    };
};
var minimum = function (dictOrd) {
    return function (dictFoldable) {
        return minimumBy(dictFoldable)(Prelude.compare(dictOrd));
    };
};
var product = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Prelude["*"](dictSemiring))(Prelude.one(dictSemiring));
    };
};
var sum = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Prelude["+"](dictSemiring))(Prelude.zero(dictSemiring));
    };
};
var foldableMultiplicative = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableMaybe = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof Data_Maybe.Just) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Foldable line 108, column 3 - line 109, column 3: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return z;
            };
            if (v1 instanceof Data_Maybe.Just) {
                return v(z)(v1.value0);
            };
            throw new Error("Failed pattern match at Data.Foldable line 106, column 3 - line 107, column 3: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return z;
            };
            if (v1 instanceof Data_Maybe.Just) {
                return v(v1.value0)(z);
            };
            throw new Error("Failed pattern match at Data.Foldable line 104, column 3 - line 105, column 3: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
});
var foldableDual = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableDisj = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableConj = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableAdditive = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldMapDefaultR = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (xs) {
                return foldr(dictFoldable)(function (x) {
                    return function (acc) {
                        return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(f(x))(acc);
                    };
                })(Data_Monoid.mempty(dictMonoid))(xs);
            };
        };
    };
};
var foldableArray = new Foldable(function (dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
}, $foreign.foldlArray, $foreign.foldrArray);
var foldMapDefaultL = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (xs) {
                return foldl(dictFoldable)(function (acc) {
                    return function (x) {
                        return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(f(x))(acc);
                    };
                })(Data_Monoid.mempty(dictMonoid))(xs);
            };
        };
    };
};
var foldMap = function (dict) {
    return dict.foldMap;
};
var foldableFirst = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return foldMap(foldableMaybe)(dictMonoid)(f)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldl(foldableMaybe)(f)(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldr(foldableMaybe)(f)(z)(v);
        };
    };
});
var foldableLast = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return foldMap(foldableMaybe)(dictMonoid)(f)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldl(foldableMaybe)(f)(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldr(foldableMaybe)(f)(z)(v);
        };
    };
});
var foldlDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Monoid_Endo.runEndo(Data_Monoid_Dual.runDual(foldMap(dictFoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo))(function ($162) {
                    return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Prelude.flip(c)($162)));
                })(xs)))(u);
            };
        };
    };
};
var foldrDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Monoid_Endo.runEndo(foldMap(dictFoldable)(Data_Monoid_Endo.monoidEndo)(function ($163) {
                    return Data_Monoid_Endo.Endo(c($163));
                })(xs))(u);
            };
        };
    };
};
var fold = function (dictFoldable) {
    return function (dictMonoid) {
        return foldMap(dictFoldable)(dictMonoid)(Prelude.id(Prelude.categoryFn));
    };
};
var find = function (dictFoldable) {
    return function (p) {
        return foldl(dictFoldable)(function (r) {
            return function (x) {
                var $160 = p(x);
                if ($160) {
                    return new Data_Maybe.Just(x);
                };
                if (!$160) {
                    return r;
                };
                throw new Error("Failed pattern match at Data.Foldable line 234, column 25 - line 234, column 50: " + [ $160.constructor.name ]);
            };
        })(Data_Maybe.Nothing.value);
    };
};
var any = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function ($164) {
                return Data_Monoid_Disj.runDisj(foldMap(dictFoldable)(Data_Monoid_Disj.monoidDisj(dictBooleanAlgebra))(function ($165) {
                    return Data_Monoid_Disj.Disj(p($165));
                })($164));
            };
        };
    };
};
var elem = function (dictFoldable) {
    return function (dictEq) {
        return function ($166) {
            return any(dictFoldable)(Prelude.booleanAlgebraBoolean)(Prelude["=="](dictEq)($166));
        };
    };
};
var notElem = function (dictFoldable) {
    return function (dictEq) {
        return function (x) {
            return function ($167) {
                return !elem(dictFoldable)(dictEq)(x)($167);
            };
        };
    };
};
var or = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return any(dictFoldable)(dictBooleanAlgebra)(Prelude.id(Prelude.categoryFn));
    };
};
var all = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function ($168) {
                return Data_Monoid_Conj.runConj(foldMap(dictFoldable)(Data_Monoid_Conj.monoidConj(dictBooleanAlgebra))(function ($169) {
                    return Data_Monoid_Conj.Conj(p($169));
                })($168));
            };
        };
    };
};
var and = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return all(dictFoldable)(dictBooleanAlgebra)(Prelude.id(Prelude.categoryFn));
    };
};
module.exports = {
    Foldable: Foldable, 
    minimumBy: minimumBy, 
    minimum: minimum, 
    maximumBy: maximumBy, 
    maximum: maximum, 
    find: find, 
    notElem: notElem, 
    elem: elem, 
    product: product, 
    sum: sum, 
    all: all, 
    any: any, 
    or: or, 
    and: and, 
    intercalate: intercalate, 
    mconcat: mconcat, 
    sequence_: sequence_, 
    for_: for_, 
    traverse_: traverse_, 
    fold: fold, 
    foldMapDefaultR: foldMapDefaultR, 
    foldMapDefaultL: foldMapDefaultL, 
    foldlDefault: foldlDefault, 
    foldrDefault: foldrDefault, 
    foldMap: foldMap, 
    foldl: foldl, 
    foldr: foldr, 
    foldableArray: foldableArray, 
    foldableMaybe: foldableMaybe, 
    foldableFirst: foldableFirst, 
    foldableLast: foldableLast, 
    foldableAdditive: foldableAdditive, 
    foldableDual: foldableDual, 
    foldableDisj: foldableDisj, 
    foldableConj: foldableConj, 
    foldableMultiplicative: foldableMultiplicative
};

},{"../Control.Apply":28,"../Data.Maybe":152,"../Data.Maybe.First":148,"../Data.Maybe.Last":149,"../Data.Monoid":159,"../Data.Monoid.Additive":153,"../Data.Monoid.Conj":154,"../Data.Monoid.Disj":155,"../Data.Monoid.Dual":156,"../Data.Monoid.Endo":157,"../Data.Monoid.Multiplicative":158,"../Prelude":203,"./foreign":119}],121:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Foreign_Index = require("../Data.Foreign.Index");
var Data_Foreign_Null = require("../Data.Foreign.Null");
var Data_Foreign_NullOrUndefined = require("../Data.Foreign.NullOrUndefined");
var Data_Foreign_Undefined = require("../Data.Foreign.Undefined");
var Data_Int = require("../Data.Int");
var Data_Traversable = require("../Data.Traversable");
var IsForeign = function (read) {
    this.read = read;
};
var stringIsForeign = new IsForeign(Data_Foreign.readString);
var read = function (dict) {
    return dict.read;
};
var readJSON = function (dictIsForeign) {
    return function (json) {
        return Prelude[">>="](Data_Either.bindEither)(Data_Foreign.parseJSON(json))(read(dictIsForeign));
    };
};
var readWith = function (dictIsForeign) {
    return function (f) {
        return function (value) {
            return Data_Either.either(function ($8) {
                return Data_Either.Left.create(f($8));
            })(Data_Either.Right.create)(read(dictIsForeign)(value));
        };
    };
};
var readProp = function (dictIsForeign) {
    return function (dictIndex) {
        return function (prop) {
            return function (value) {
                return Prelude[">>="](Data_Either.bindEither)(Data_Foreign_Index["!"](dictIndex)(value)(prop))(readWith(dictIsForeign)(Data_Foreign_Index.errorAt(dictIndex)(prop)));
            };
        };
    };
};
var undefinedIsForeign = function (dictIsForeign) {
    return new IsForeign(Data_Foreign_Undefined.readUndefined(read(dictIsForeign)));
};
var numberIsForeign = new IsForeign(Data_Foreign.readNumber);
var nullOrUndefinedIsForeign = function (dictIsForeign) {
    return new IsForeign(Data_Foreign_NullOrUndefined.readNullOrUndefined(read(dictIsForeign)));
};
var nullIsForeign = function (dictIsForeign) {
    return new IsForeign(Data_Foreign_Null.readNull(read(dictIsForeign)));
};
var intIsForeign = new IsForeign(Data_Foreign.readInt);
var foreignIsForeign = new IsForeign(function (f) {
    return Prelude["return"](Data_Either.applicativeEither)(f);
});
var charIsForeign = new IsForeign(Data_Foreign.readChar);
var booleanIsForeign = new IsForeign(Data_Foreign.readBoolean);
var arrayIsForeign = function (dictIsForeign) {
    return new IsForeign(function (value) {
        var readElement = function (i) {
            return function (value1) {
                return readWith(dictIsForeign)(Data_Foreign.ErrorAtIndex.create(i))(value1);
            };
        };
        var readElements = function (arr) {
            return Data_Traversable.sequence(Data_Traversable.traversableArray)(Data_Either.applicativeEither)(Data_Array.zipWith(readElement)(Data_Array.range(0)(Data_Array.length(arr)))(arr));
        };
        return Prelude[">>="](Data_Either.bindEither)(Data_Foreign.readArray(value))(readElements);
    });
};
module.exports = {
    IsForeign: IsForeign, 
    readProp: readProp, 
    readWith: readWith, 
    readJSON: readJSON, 
    read: read, 
    foreignIsForeign: foreignIsForeign, 
    stringIsForeign: stringIsForeign, 
    charIsForeign: charIsForeign, 
    booleanIsForeign: booleanIsForeign, 
    numberIsForeign: numberIsForeign, 
    intIsForeign: intIsForeign, 
    arrayIsForeign: arrayIsForeign, 
    nullIsForeign: nullIsForeign, 
    undefinedIsForeign: undefinedIsForeign, 
    nullOrUndefinedIsForeign: nullOrUndefinedIsForeign
};

},{"../Data.Array":102,"../Data.Either":114,"../Data.Foreign":128,"../Data.Foreign.Index":123,"../Data.Foreign.Null":124,"../Data.Foreign.NullOrUndefined":125,"../Data.Foreign.Undefined":126,"../Data.Int":143,"../Data.Traversable":170,"../Prelude":203}],122:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Foreign.Index

// jshint maxparams: 4
exports.unsafeReadPropImpl = function (f, s, key, value) {
  return value == null ? f : s(value[key]);
};

// jshint maxparams: 2
exports.unsafeHasOwnProperty = function (prop, value) {
  return Object.prototype.hasOwnProperty.call(value, prop);
};

exports.unsafeHasProperty = function (prop, value) {
  return prop in value;
};

},{}],123:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Function = require("../Data.Function");
var Data_Int = require("../Data.Int");
var Index = function (errorAt, hasOwnProperty, hasProperty, ix) {
    this.errorAt = errorAt;
    this.hasOwnProperty = hasOwnProperty;
    this.hasProperty = hasProperty;
    this.ix = ix;
};
var unsafeReadProp = function (k) {
    return function (value) {
        return $foreign.unsafeReadPropImpl(new Data_Either.Left(new Data_Foreign.TypeMismatch("object", Data_Foreign.typeOf(value))), Prelude.pure(Data_Either.applicativeEither), k, value);
    };
};
var prop = unsafeReadProp;
var ix = function (dict) {
    return dict.ix;
};
var $bang = function (dictIndex) {
    return ix(dictIndex);
};
var index = unsafeReadProp;
var hasPropertyImpl = function (v) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return false;
        };
        if (Data_Foreign.isUndefined(value)) {
            return false;
        };
        if (Data_Foreign.typeOf(value) === "object" || Data_Foreign.typeOf(value) === "function") {
            return $foreign.unsafeHasProperty(v, value);
        };
        return false;
    };
};
var hasProperty = function (dict) {
    return dict.hasProperty;
};
var hasOwnPropertyImpl = function (v) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return false;
        };
        if (Data_Foreign.isUndefined(value)) {
            return false;
        };
        if (Data_Foreign.typeOf(value) === "object" || Data_Foreign.typeOf(value) === "function") {
            return $foreign.unsafeHasOwnProperty(v, value);
        };
        return false;
    };
};
var indexInt = new Index(Data_Foreign.ErrorAtIndex.create, hasOwnPropertyImpl, hasPropertyImpl, Prelude.flip(index));
var indexString = new Index(Data_Foreign.ErrorAtProperty.create, hasOwnPropertyImpl, hasPropertyImpl, Prelude.flip(prop));
var hasOwnProperty = function (dict) {
    return dict.hasOwnProperty;
};
var errorAt = function (dict) {
    return dict.errorAt;
};
module.exports = {
    Index: Index, 
    errorAt: errorAt, 
    hasOwnProperty: hasOwnProperty, 
    hasProperty: hasProperty, 
    "!": $bang, 
    ix: ix, 
    index: index, 
    prop: prop, 
    indexString: indexString, 
    indexInt: indexInt
};

},{"../Data.Either":114,"../Data.Foreign":128,"../Data.Function":130,"../Data.Int":143,"../Prelude":203,"./foreign":122}],124:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Foreign = require("../Data.Foreign");
var Data_Either = require("../Data.Either");
var Null = function (x) {
    return x;
};
var runNull = function (v) {
    return v;
};
var readNull = function (v) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return Prelude.pure(Data_Either.applicativeEither)(Data_Maybe.Nothing.value);
        };
        return Prelude["<$>"](Data_Either.functorEither)(function ($5) {
            return Null(Data_Maybe.Just.create($5));
        })(v(value));
    };
};
module.exports = {
    Null: Null, 
    readNull: readNull, 
    runNull: runNull
};

},{"../Data.Either":114,"../Data.Foreign":128,"../Data.Maybe":152,"../Prelude":203}],125:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Foreign = require("../Data.Foreign");
var Data_Either = require("../Data.Either");
var NullOrUndefined = function (x) {
    return x;
};
var runNullOrUndefined = function (v) {
    return v;
};
var readNullOrUndefined = function (v) {
    return function (value) {
        if (Data_Foreign.isNull(value) || Data_Foreign.isUndefined(value)) {
            return Prelude.pure(Data_Either.applicativeEither)(Data_Maybe.Nothing.value);
        };
        return Prelude["<$>"](Data_Either.functorEither)(function ($5) {
            return NullOrUndefined(Data_Maybe.Just.create($5));
        })(v(value));
    };
};
module.exports = {
    NullOrUndefined: NullOrUndefined, 
    readNullOrUndefined: readNullOrUndefined, 
    runNullOrUndefined: runNullOrUndefined
};

},{"../Data.Either":114,"../Data.Foreign":128,"../Data.Maybe":152,"../Prelude":203}],126:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Foreign = require("../Data.Foreign");
var Data_Either = require("../Data.Either");
var Undefined = function (x) {
    return x;
};
var runUndefined = function (v) {
    return v;
};
var readUndefined = function (v) {
    return function (value) {
        if (Data_Foreign.isUndefined(value)) {
            return Prelude.pure(Data_Either.applicativeEither)(Data_Maybe.Nothing.value);
        };
        return Prelude["<$>"](Data_Either.functorEither)(function ($5) {
            return Undefined(Data_Maybe.Just.create($5));
        })(v(value));
    };
};
module.exports = {
    Undefined: Undefined, 
    readUndefined: readUndefined, 
    runUndefined: runUndefined
};

},{"../Data.Either":114,"../Data.Foreign":128,"../Data.Maybe":152,"../Prelude":203}],127:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Foreign

// jshint maxparams: 3
exports.parseJSONImpl = function (left, right, str) {
  try {
    return right(JSON.parse(str));
  } catch (e) {
    return left(e.toString());
  }
};

// jshint maxparams: 1
exports.toForeign = function (value) {
  return value;
};

exports.unsafeFromForeign = function (value) {
  return value;
};

exports.typeOf = function (value) {
  return typeof value;
};

exports.tagOf = function (value) {
  return Object.prototype.toString.call(value).slice(8, -1);
};

exports.isNull = function (value) {
  return value === null;
};

exports.isUndefined = function (value) {
  return value === undefined;
};

exports.isArray = Array.isArray || function (value) {
  return Object.prototype.toString.call(value) === "[object Array]";
};

},{}],128:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Maybe = require("../Data.Maybe");
var Data_Function = require("../Data.Function");
var Data_Int_1 = require("../Data.Int");
var Data_Int_1 = require("../Data.Int");
var Data_String = require("../Data.String");
var TypeMismatch = (function () {
    function TypeMismatch(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TypeMismatch.create = function (value0) {
        return function (value1) {
            return new TypeMismatch(value0, value1);
        };
    };
    return TypeMismatch;
})();
var ErrorAtIndex = (function () {
    function ErrorAtIndex(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtIndex.create = function (value0) {
        return function (value1) {
            return new ErrorAtIndex(value0, value1);
        };
    };
    return ErrorAtIndex;
})();
var ErrorAtProperty = (function () {
    function ErrorAtProperty(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtProperty.create = function (value0) {
        return function (value1) {
            return new ErrorAtProperty(value0, value1);
        };
    };
    return ErrorAtProperty;
})();
var JSONError = (function () {
    function JSONError(value0) {
        this.value0 = value0;
    };
    JSONError.create = function (value0) {
        return new JSONError(value0);
    };
    return JSONError;
})();
var unsafeReadTagged = function (tag) {
    return function (value) {
        if ($foreign.tagOf(value) === tag) {
            return Prelude.pure(Data_Either.applicativeEither)($foreign.unsafeFromForeign(value));
        };
        return new Data_Either.Left(new TypeMismatch(tag, $foreign.tagOf(value)));
    };
};
var showForeignError = new Prelude.Show(function (v) {
    if (v instanceof TypeMismatch) {
        return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    };
    if (v instanceof ErrorAtIndex) {
        return "Error at array index " + (Prelude.show(Prelude.showInt)(v.value0) + (": " + Prelude.show(showForeignError)(v.value1)));
    };
    if (v instanceof ErrorAtProperty) {
        return "Error at property " + (Prelude.show(Prelude.showString)(v.value0) + (": " + Prelude.show(showForeignError)(v.value1)));
    };
    if (v instanceof JSONError) {
        return "JSON error: " + v.value0;
    };
    throw new Error("Failed pattern match at Data.Foreign line 54, column 3 - line 55, column 3: " + [ v.constructor.name ]);
});
var readString = unsafeReadTagged("String");
var readNumber = unsafeReadTagged("Number");
var readInt = function (value) {
    var error = Data_Either.Left.create(new TypeMismatch("Int", $foreign.tagOf(value)));
    var fromNumber = function ($30) {
        return Data_Maybe.maybe(error)(Prelude.pure(Data_Either.applicativeEither))(Data_Int_1.fromNumber($30));
    };
    return Data_Either.either(Prelude["const"](error))(fromNumber)(readNumber(value));
};
var readChar = function (value) {
    var error = Data_Either.Left.create(new TypeMismatch("Char", $foreign.tagOf(value)));
    var fromString = function ($31) {
        return Data_Maybe.maybe(error)(Prelude.pure(Data_Either.applicativeEither))(Data_String.toChar($31));
    };
    return Data_Either.either(Prelude["const"](error))(fromString)(readString(value));
};
var readBoolean = unsafeReadTagged("Boolean");
var readArray = function (value) {
    if ($foreign.isArray(value)) {
        return Prelude.pure(Data_Either.applicativeEither)($foreign.unsafeFromForeign(value));
    };
    return new Data_Either.Left(new TypeMismatch("array", $foreign.tagOf(value)));
};
var parseJSON = function (json) {
    return $foreign.parseJSONImpl(function ($32) {
        return Data_Either.Left.create(JSONError.create($32));
    }, Data_Either.Right.create, json);
};
var eqForeignError = new Prelude.Eq(function (v) {
    return function (v1) {
        if (v instanceof TypeMismatch && v1 instanceof TypeMismatch) {
            return v.value0 === v1.value0 && v.value1 === v1.value1;
        };
        if (v instanceof ErrorAtIndex && v1 instanceof ErrorAtIndex) {
            return v.value0 === v1.value0 && Prelude["=="](eqForeignError)(v.value1)(v1.value1);
        };
        if (v instanceof ErrorAtProperty && v1 instanceof ErrorAtProperty) {
            return v.value0 === v1.value0 && Prelude["=="](eqForeignError)(v.value1)(v1.value1);
        };
        if (v instanceof JSONError && v1 instanceof JSONError) {
            return v.value0 === v1.value0;
        };
        return false;
    };
});
module.exports = {
    TypeMismatch: TypeMismatch, 
    ErrorAtIndex: ErrorAtIndex, 
    ErrorAtProperty: ErrorAtProperty, 
    JSONError: JSONError, 
    readArray: readArray, 
    readInt: readInt, 
    readNumber: readNumber, 
    readBoolean: readBoolean, 
    readChar: readChar, 
    readString: readString, 
    unsafeReadTagged: unsafeReadTagged, 
    parseJSON: parseJSON, 
    showForeignError: showForeignError, 
    eqForeignError: eqForeignError, 
    isArray: $foreign.isArray, 
    isUndefined: $foreign.isUndefined, 
    isNull: $foreign.isNull, 
    tagOf: $foreign.tagOf, 
    typeOf: $foreign.typeOf, 
    unsafeFromForeign: $foreign.unsafeFromForeign, 
    toForeign: $foreign.toForeign
};

},{"../Data.Either":114,"../Data.Function":130,"../Data.Int":143,"../Data.Maybe":152,"../Data.String":168,"../Prelude":203,"./foreign":127}],129:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Function

exports.mkFn0 = function (fn) {
  return function () {
    return fn({});
  };
};

exports.mkFn1 = function (fn) {
  return function (a) {
    return fn(a);
  };
};

exports.mkFn2 = function (fn) {
  /* jshint maxparams: 2 */
  return function (a, b) {
    return fn(a)(b);
  };
};

exports.mkFn3 = function (fn) {
  /* jshint maxparams: 3 */
  return function (a, b, c) {
    return fn(a)(b)(c);
  };
};

exports.mkFn4 = function (fn) {
  /* jshint maxparams: 4 */
  return function (a, b, c, d) {
    return fn(a)(b)(c)(d);
  };
};

exports.mkFn5 = function (fn) {
  /* jshint maxparams: 5 */
  return function (a, b, c, d, e) {
    return fn(a)(b)(c)(d)(e);
  };
};

exports.mkFn6 = function (fn) {
  /* jshint maxparams: 6 */
  return function (a, b, c, d, e, f) {
    return fn(a)(b)(c)(d)(e)(f);
  };
};

exports.mkFn7 = function (fn) {
  /* jshint maxparams: 7 */
  return function (a, b, c, d, e, f, g) {
    return fn(a)(b)(c)(d)(e)(f)(g);
  };
};

exports.mkFn8 = function (fn) {
  /* jshint maxparams: 8 */
  return function (a, b, c, d, e, f, g, h) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h);
  };
};

exports.mkFn9 = function (fn) {
  /* jshint maxparams: 9 */
  return function (a, b, c, d, e, f, g, h, i) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i);
  };
};

exports.mkFn10 = function (fn) {
  /* jshint maxparams: 10 */
  return function (a, b, c, d, e, f, g, h, i, j) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j);
  };
};

exports.runFn0 = function (fn) {
  return fn();
};

exports.runFn1 = function (fn) {
  return function (a) {
    return fn(a);
  };
};

exports.runFn2 = function (fn) {
  return function (a) {
    return function (b) {
      return fn(a, b);
    };
  };
};

exports.runFn3 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return fn(a, b, c);
      };
    };
  };
};

exports.runFn4 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return fn(a, b, c, d);
        };
      };
    };
  };
};

exports.runFn5 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return fn(a, b, c, d, e);
          };
        };
      };
    };
  };
};

exports.runFn6 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return fn(a, b, c, d, e, f);
            };
          };
        };
      };
    };
  };
};

exports.runFn7 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return fn(a, b, c, d, e, f, g);
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn8 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return fn(a, b, c, d, e, f, g, h);
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn9 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return fn(a, b, c, d, e, f, g, h, i);
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn10 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return function (j) {
                      return fn(a, b, c, d, e, f, g, h, i, j);
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

},{}],130:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var on = function (f) {
    return function (g) {
        return function (x) {
            return function (y) {
                return f(g(x))(g(y));
            };
        };
    };
};
module.exports = {
    on: on, 
    runFn10: $foreign.runFn10, 
    runFn9: $foreign.runFn9, 
    runFn8: $foreign.runFn8, 
    runFn7: $foreign.runFn7, 
    runFn6: $foreign.runFn6, 
    runFn5: $foreign.runFn5, 
    runFn4: $foreign.runFn4, 
    runFn3: $foreign.runFn3, 
    runFn2: $foreign.runFn2, 
    runFn1: $foreign.runFn1, 
    runFn0: $foreign.runFn0, 
    mkFn10: $foreign.mkFn10, 
    mkFn9: $foreign.mkFn9, 
    mkFn8: $foreign.mkFn8, 
    mkFn7: $foreign.mkFn7, 
    mkFn6: $foreign.mkFn6, 
    mkFn5: $foreign.mkFn5, 
    mkFn4: $foreign.mkFn4, 
    mkFn3: $foreign.mkFn3, 
    mkFn2: $foreign.mkFn2, 
    mkFn1: $foreign.mkFn1, 
    mkFn0: $foreign.mkFn0
};

},{"../Prelude":203,"./foreign":129}],131:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Contravariant = function (cmap) {
    this.cmap = cmap;
};
var cmap = function (dict) {
    return dict.cmap;
};
var $greater$dollar$less = function (dictContravariant) {
    return cmap(dictContravariant);
};
var $greater$hash$less = function (dictContravariant) {
    return function (x) {
        return function (f) {
            return $greater$dollar$less(dictContravariant)(f)(x);
        };
    };
};
module.exports = {
    Contravariant: Contravariant, 
    ">#<": $greater$hash$less, 
    ">$<": $greater$dollar$less, 
    cmap: cmap
};

},{}],132:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Traversable = require("../Data.Traversable");
var Coproduct = function (x) {
    return x;
};
var runCoproduct = function (v) {
    return v;
};
var right = function ($11) {
    return Coproduct(Data_Either.Right.create($11));
};
var left = function ($12) {
    return Coproduct(Data_Either.Left.create($12));
};
var coproduct = function (f) {
    return function (g) {
        return function ($13) {
            return Data_Either.either(f)(g)(runCoproduct($13));
        };
    };
};
var foldableCoproduct = function (dictFoldable) {
    return function (dictFoldable1) {
        return new Data_Foldable.Foldable(function (dictMonoid) {
            return function (f) {
                return coproduct(Data_Foldable.foldMap(dictFoldable)(dictMonoid)(f))(Data_Foldable.foldMap(dictFoldable1)(dictMonoid)(f));
            };
        }, function (f) {
            return function (z) {
                return coproduct(Data_Foldable.foldl(dictFoldable)(f)(z))(Data_Foldable.foldl(dictFoldable1)(f)(z));
            };
        }, function (f) {
            return function (z) {
                return coproduct(Data_Foldable.foldr(dictFoldable)(f)(z))(Data_Foldable.foldr(dictFoldable1)(f)(z));
            };
        });
    };
};
var functorCoproduct = function (dictFunctor) {
    return function (dictFunctor1) {
        return new Prelude.Functor(function (f) {
            return function ($14) {
                return Coproduct(coproduct(function ($15) {
                    return Data_Either.Left.create(Prelude["<$>"](dictFunctor)(f)($15));
                })(function ($16) {
                    return Data_Either.Right.create(Prelude["<$>"](dictFunctor1)(f)($16));
                })($14));
            };
        });
    };
};
var traversableCoproduct = function (dictTraversable) {
    return function (dictTraversable1) {
        return new Data_Traversable.Traversable(function () {
            return foldableCoproduct(dictTraversable["__superclass_Data.Foldable.Foldable_1"]())(dictTraversable1["__superclass_Data.Foldable.Foldable_1"]());
        }, function () {
            return functorCoproduct(dictTraversable["__superclass_Prelude.Functor_0"]())(dictTraversable1["__superclass_Prelude.Functor_0"]());
        }, function (dictApplicative) {
            return coproduct(function ($17) {
                return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function ($18) {
                    return Coproduct(Data_Either.Left.create($18));
                })(Data_Traversable.sequence(dictTraversable)(dictApplicative)($17));
            })(function ($19) {
                return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function ($20) {
                    return Coproduct(Data_Either.Right.create($20));
                })(Data_Traversable.sequence(dictTraversable1)(dictApplicative)($19));
            });
        }, function (dictApplicative) {
            return function (f) {
                return coproduct(function ($21) {
                    return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function ($22) {
                        return Coproduct(Data_Either.Left.create($22));
                    })(Data_Traversable.traverse(dictTraversable)(dictApplicative)(f)($21));
                })(function ($23) {
                    return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function ($24) {
                        return Coproduct(Data_Either.Right.create($24));
                    })(Data_Traversable.traverse(dictTraversable1)(dictApplicative)(f)($23));
                });
            };
        });
    };
};
module.exports = {
    Coproduct: Coproduct, 
    coproduct: coproduct, 
    right: right, 
    left: left, 
    runCoproduct: runCoproduct, 
    functorCoproduct: functorCoproduct, 
    foldableCoproduct: foldableCoproduct, 
    traversableCoproduct: traversableCoproduct
};

},{"../Data.Either":114,"../Data.Foldable":120,"../Data.Traversable":170,"../Prelude":203}],133:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Invariant = function (imap) {
    this.imap = imap;
};
var imapF = function (dictFunctor) {
    return function ($1) {
        return Prelude["const"](Prelude.map(dictFunctor)($1));
    };
};
var invariantArray = new Invariant(imapF(Prelude.functorArray));
var invariantFn = new Invariant(imapF(Prelude.functorFn));
var imap = function (dict) {
    return dict.imap;
};
module.exports = {
    Invariant: Invariant, 
    imapF: imapF, 
    imap: imap, 
    invariantFn: invariantFn, 
    invariantArray: invariantArray
};

},{"../Prelude":203}],134:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var $less$dollar = function (dictFunctor) {
    return function (x) {
        return function (f) {
            return Prelude["<$>"](dictFunctor)(Prelude["const"](x))(f);
        };
    };
};
var $dollar$greater = function (dictFunctor) {
    return function (f) {
        return function (x) {
            return Prelude["<$>"](dictFunctor)(Prelude["const"](x))(f);
        };
    };
};
module.exports = {
    "$>": $dollar$greater, 
    "<$": $less$dollar
};

},{"../Prelude":203}],135:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Generic


//------------------------------------------------------------------------------
// Zipping ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.zipAll = function(f) {
    return function(xs) {
        return function(ys) {
            var l = xs.length < ys.length ? xs.length : ys.length;
            for (var i = 0; i < l; i++) {
                if (!f(xs[i])(ys[i])) {
                    return false;
                }
            }
            return true;
        };
    };
};

exports.zipCompare = function(f) {
    return function(xs) {
        return function(ys) {
            var i = 0;
            var xlen = xs.length;
            var ylen = ys.length;
            while (i < xlen && i < ylen) {
                var o = f(xs[i])(ys[i]);
                if (o !== 0) {
                    return o;
                }
                i++;
            }
            if (xlen === ylen) {
                return 0;
            } else if (xlen > ylen) {
                return -1;
            } else {
                return 1;
            }
        };
    };
};

},{}],136:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Maybe = require("../Data.Maybe");
var Data_Tuple = require("../Data.Tuple");
var Data_Traversable = require("../Data.Traversable");
var Data_Foldable = require("../Data.Foldable");
var Data_Array = require("../Data.Array");
var Data_String = require("../Data.String");
var Type_Proxy = require("../Type.Proxy");
var Data_Monoid = require("../Data.Monoid");
var SProd = (function () {
    function SProd(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SProd.create = function (value0) {
        return function (value1) {
            return new SProd(value0, value1);
        };
    };
    return SProd;
})();
var SRecord = (function () {
    function SRecord(value0) {
        this.value0 = value0;
    };
    SRecord.create = function (value0) {
        return new SRecord(value0);
    };
    return SRecord;
})();
var SNumber = (function () {
    function SNumber(value0) {
        this.value0 = value0;
    };
    SNumber.create = function (value0) {
        return new SNumber(value0);
    };
    return SNumber;
})();
var SBoolean = (function () {
    function SBoolean(value0) {
        this.value0 = value0;
    };
    SBoolean.create = function (value0) {
        return new SBoolean(value0);
    };
    return SBoolean;
})();
var SInt = (function () {
    function SInt(value0) {
        this.value0 = value0;
    };
    SInt.create = function (value0) {
        return new SInt(value0);
    };
    return SInt;
})();
var SString = (function () {
    function SString(value0) {
        this.value0 = value0;
    };
    SString.create = function (value0) {
        return new SString(value0);
    };
    return SString;
})();
var SChar = (function () {
    function SChar(value0) {
        this.value0 = value0;
    };
    SChar.create = function (value0) {
        return new SChar(value0);
    };
    return SChar;
})();
var SArray = (function () {
    function SArray(value0) {
        this.value0 = value0;
    };
    SArray.create = function (value0) {
        return new SArray(value0);
    };
    return SArray;
})();
var SigProd = (function () {
    function SigProd(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SigProd.create = function (value0) {
        return function (value1) {
            return new SigProd(value0, value1);
        };
    };
    return SigProd;
})();
var SigRecord = (function () {
    function SigRecord(value0) {
        this.value0 = value0;
    };
    SigRecord.create = function (value0) {
        return new SigRecord(value0);
    };
    return SigRecord;
})();
var SigNumber = (function () {
    function SigNumber() {

    };
    SigNumber.value = new SigNumber();
    return SigNumber;
})();
var SigBoolean = (function () {
    function SigBoolean() {

    };
    SigBoolean.value = new SigBoolean();
    return SigBoolean;
})();
var SigInt = (function () {
    function SigInt() {

    };
    SigInt.value = new SigInt();
    return SigInt;
})();
var SigString = (function () {
    function SigString() {

    };
    SigString.value = new SigString();
    return SigString;
})();
var SigChar = (function () {
    function SigChar() {

    };
    SigChar.value = new SigChar();
    return SigChar;
})();
var SigArray = (function () {
    function SigArray(value0) {
        this.value0 = value0;
    };
    SigArray.create = function (value0) {
        return new SigArray(value0);
    };
    return SigArray;
})();
var Generic = function (fromSpine, toSignature, toSpine) {
    this.fromSpine = fromSpine;
    this.toSignature = toSignature;
    this.toSpine = toSpine;
};
var toSpine = function (dict) {
    return dict.toSpine;
};
var toSignature = function (dict) {
    return dict.toSignature;
};
var showArray = function (f) {
    return function (xs) {
        return "[ " + (Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(", ")(Prelude.map(Prelude.functorArray)(f)(xs)) + " ]");
    };
};
var isValidSpine = function (v) {
    return function (v1) {
        if (v instanceof SigBoolean && v1 instanceof SBoolean) {
            return true;
        };
        if (v instanceof SigNumber && v1 instanceof SNumber) {
            return true;
        };
        if (v instanceof SigInt && v1 instanceof SInt) {
            return true;
        };
        if (v instanceof SigString && v1 instanceof SString) {
            return true;
        };
        if (v instanceof SigChar && v1 instanceof SChar) {
            return true;
        };
        if (v instanceof SigArray && v1 instanceof SArray) {
            return Data_Foldable.all(Data_Foldable.foldableArray)(Prelude.booleanAlgebraBoolean)(function ($193) {
                return isValidSpine(v.value0(Prelude.unit))((function (v2) {
                    return v2(Prelude.unit);
                })($193));
            })(v1.value0);
        };
        if (v instanceof SigProd && v1 instanceof SProd) {
            var $54 = Data_Foldable.find(Data_Foldable.foldableArray)(function ($194) {
                return (function (v2) {
                    return v1.value0 === v2;
                })((function (v2) {
                    return v2.sigConstructor;
                })($194));
            })(v.value1);
            if ($54 instanceof Data_Maybe.Nothing) {
                return false;
            };
            if ($54 instanceof Data_Maybe.Just) {
                return Data_Foldable.and(Data_Foldable.foldableArray)(Prelude.booleanAlgebraBoolean)(Data_Array.zipWith(function (sig) {
                    return function (spine) {
                        return isValidSpine(sig(Prelude.unit))(spine(Prelude.unit));
                    };
                })($54.value0.sigValues)(v1.value1));
            };
            throw new Error("Failed pattern match at Data.Generic line 132, column 3 - line 136, column 1: " + [ $54.constructor.name ]);
        };
        if (v instanceof SigRecord && v1 instanceof SRecord) {
            return Data_Foldable.and(Data_Foldable.foldableArray)(Prelude.booleanAlgebraBoolean)(Data_Array.zipWith(function (sig) {
                return function (val) {
                    return isValidSpine(sig.recValue(Prelude.unit))(val.recValue(Prelude.unit));
                };
            })(Data_Array.sortBy(function (a) {
                return function (b) {
                    return Prelude.compare(Prelude.ordString)(a.recLabel)(b.recLabel);
                };
            })(v.value0))(Data_Array.sortBy(function (a) {
                return function (b) {
                    return Prelude.compare(Prelude.ordString)(a.recLabel)(b.recLabel);
                };
            })(v1.value0)));
        };
        return false;
    };
};
var genericString = new Generic(function (v) {
    if (v instanceof SString) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigString.value;
}, function (x) {
    return new SString(x);
});
var genericShowPrec = function (d) {
    return function (v) {
        if (v instanceof SProd) {
            var showParen = function (v1) {
                return function (x) {
                    if (!v1) {
                        return x;
                    };
                    if (v1) {
                        return "(" + (x + ")");
                    };
                    throw new Error("Failed pattern match at Data.Generic line 240, column 9 - line 241, column 9: " + [ v1.constructor.name, x.constructor.name ]);
                };
            };
            var $69 = Data_Array["null"](v.value1);
            if ($69) {
                return v.value0;
            };
            if (!$69) {
                return showParen(d > 10)(v.value0 + (" " + Data_String.joinWith(" ")(Prelude.map(Prelude.functorArray)(function (x) {
                    return genericShowPrec(11)(x(Prelude.unit));
                })(v.value1))));
            };
            throw new Error("Failed pattern match at Data.Generic line 237, column 5 - line 240, column 3: " + [ $69.constructor.name ]);
        };
        if (v instanceof SRecord) {
            return "{" + (Data_String.joinWith(", ")(Prelude.map(Prelude.functorArray)(function (x) {
                return x.recLabel + (": " + genericShowPrec(0)(x.recValue(Prelude.unit)));
            })(v.value0)) + "}");
        };
        if (v instanceof SBoolean) {
            return Prelude.show(Prelude.showBoolean)(v.value0);
        };
        if (v instanceof SInt) {
            return Prelude.show(Prelude.showInt)(v.value0);
        };
        if (v instanceof SNumber) {
            return Prelude.show(Prelude.showNumber)(v.value0);
        };
        if (v instanceof SString) {
            return Prelude.show(Prelude.showString)(v.value0);
        };
        if (v instanceof SChar) {
            return Prelude.show(Prelude.showChar)(v.value0);
        };
        if (v instanceof SArray) {
            return "[" + (Data_String.joinWith(", ")(Prelude.map(Prelude.functorArray)(function (x) {
                return genericShowPrec(0)(x(Prelude.unit));
            })(v.value0)) + "]");
        };
        throw new Error("Failed pattern match at Data.Generic line 236, column 1 - line 243, column 1: " + [ d.constructor.name, v.constructor.name ]);
    };
};
var genericNumber = new Generic(function (v) {
    if (v instanceof SNumber) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigNumber.value;
}, function (x) {
    return new SNumber(x);
});
var genericInt = new Generic(function (v) {
    if (v instanceof SInt) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigInt.value;
}, function (x) {
    return new SInt(x);
});
var genericChar = new Generic(function (v) {
    if (v instanceof SChar) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigChar.value;
}, function (x) {
    return new SChar(x);
});
var genericBool = new Generic(function (v) {
    if (v instanceof SBoolean) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigBoolean.value;
}, function (b) {
    return new SBoolean(b);
});
var gShow = function (dictGeneric) {
    return function ($195) {
        return genericShowPrec(0)(toSpine(dictGeneric)($195));
    };
};
var fromSpine = function (dict) {
    return dict.fromSpine;
};
var force = function (v) {
    return v(Prelude.unit);
};
var showSignature = function (sig) {
    var needsParen = function (s) {
        if (s instanceof SigProd) {
            return true;
        };
        if (s instanceof SigRecord) {
            return true;
        };
        if (s instanceof SigNumber) {
            return false;
        };
        if (s instanceof SigBoolean) {
            return false;
        };
        if (s instanceof SigInt) {
            return false;
        };
        if (s instanceof SigString) {
            return false;
        };
        if (s instanceof SigChar) {
            return false;
        };
        if (s instanceof SigArray) {
            return true;
        };
        throw new Error("Failed pattern match at Data.Generic line 79, column 18 - line 89, column 1: " + [ s.constructor.name ]);
    };
    var paren = function (s) {
        if (needsParen(s)) {
            return "(" + (showSignature(s) + ")");
        };
        if (Prelude.otherwise) {
            return showSignature(s);
        };
        throw new Error("Failed pattern match at Data.Generic line 55, column 1 - line 89, column 1: " + [ s.constructor.name ]);
    };
    return Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Monoid.monoidString)((function () {
        if (sig instanceof SigProd) {
            return [ "SigProd ", Prelude.show(Prelude.showString)(sig.value0), " ", showArray(showDataConstructor)(sig.value1) ];
        };
        if (sig instanceof SigRecord) {
            return [ "SigRecord ", showArray(showLabel)(sig.value0) ];
        };
        if (sig instanceof SigNumber) {
            return [ "SigNumber" ];
        };
        if (sig instanceof SigBoolean) {
            return [ "SigBoolean" ];
        };
        if (sig instanceof SigInt) {
            return [ "SigInt" ];
        };
        if (sig instanceof SigString) {
            return [ "SigString" ];
        };
        if (sig instanceof SigChar) {
            return [ "SigChar" ];
        };
        if (sig instanceof SigArray) {
            return [ "SigArray ", paren(force(sig.value0)) ];
        };
        throw new Error("Failed pattern match at Data.Generic line 56, column 3 - line 74, column 3: " + [ sig.constructor.name ]);
    })());
};
var showLabel = function (l) {
    return "{ recLabel: " + (Prelude.show(Prelude.showString)(l.recLabel) + (", recValue: " + (showSignature(force(l.recValue)) + " }")));
};
var showDataConstructor = function (dc) {
    return "{ sigConstructor: " + (Prelude.show(Prelude.showString)(dc.sigConstructor) + (", sigValues: " + (showArray(function ($196) {
        return showSignature(force($196));
    })(dc.sigValues) + "}")));
};
var showGenericSignature = new Prelude.Show(showSignature);
var eqGeneric = new Prelude.Eq(function (v) {
    return function (v1) {
        if (v instanceof SProd && v1 instanceof SProd) {
            return v.value0 === v1.value0 && (Data_Array.length(v.value1) === Data_Array.length(v1.value1) && $foreign.zipAll(function (x) {
                return function (y) {
                    return Prelude["=="](eqGeneric)(x(Prelude.unit))(y(Prelude.unit));
                };
            })(v.value1)(v1.value1));
        };
        if (v instanceof SRecord && v1 instanceof SRecord) {
            var go = function (x) {
                return function (y) {
                    return x.recLabel === y.recLabel && Prelude["=="](eqGeneric)(x.recValue(Prelude.unit))(y.recValue(Prelude.unit));
                };
            };
            return Data_Array.length(v.value0) === Data_Array.length(v1.value0) && $foreign.zipAll(go)(v.value0)(v1.value0);
        };
        if (v instanceof SInt && v1 instanceof SInt) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SNumber && v1 instanceof SNumber) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SBoolean && v1 instanceof SBoolean) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SChar && v1 instanceof SChar) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SString && v1 instanceof SString) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SArray && v1 instanceof SArray) {
            return Data_Array.length(v.value0) === Data_Array.length(v1.value0) && $foreign.zipAll(function (x) {
                return function (y) {
                    return Prelude["=="](eqGeneric)(x(Prelude.unit))(y(Prelude.unit));
                };
            })(v.value0)(v1.value0);
        };
        return false;
    };
});
var gEq = function (dictGeneric) {
    return function (x) {
        return function (y) {
            return Prelude["=="](eqGeneric)(toSpine(dictGeneric)(x))(toSpine(dictGeneric)(y));
        };
    };
};
var ordGeneric = new Prelude.Ord(function () {
    return eqGeneric;
}, function (v) {
    return function (v1) {
        if (v instanceof SProd && v1 instanceof SProd) {
            var $120 = Prelude.compare(Prelude.ordString)(v.value0)(v1.value0);
            if ($120 instanceof Prelude.EQ) {
                return Prelude.compare(Prelude.ordInt)(0)($foreign.zipCompare(function (x) {
                    return function (y) {
                        var $121 = Prelude.compare(ordGeneric)(x(Prelude.unit))(y(Prelude.unit));
                        if ($121 instanceof Prelude.EQ) {
                            return 0;
                        };
                        if ($121 instanceof Prelude.LT) {
                            return 1;
                        };
                        if ($121 instanceof Prelude.GT) {
                            return -1;
                        };
                        throw new Error("Failed pattern match at Data.Generic line 280, column 49 - line 283, column 59: " + [ $121.constructor.name ]);
                    };
                })(v.value1)(v1.value1));
            };
            return $120;
        };
        if (v instanceof SProd) {
            return Prelude.LT.value;
        };
        if (v1 instanceof SProd) {
            return Prelude.GT.value;
        };
        if (v instanceof SRecord && v1 instanceof SRecord) {
            var go = function (x) {
                return function (y) {
                    var $130 = Prelude.compare(Prelude.ordString)(x.recLabel)(y.recLabel);
                    if ($130 instanceof Prelude.EQ) {
                        var $131 = Prelude.compare(ordGeneric)(x.recValue(Prelude.unit))(y.recValue(Prelude.unit));
                        if ($131 instanceof Prelude.EQ) {
                            return 0;
                        };
                        if ($131 instanceof Prelude.LT) {
                            return 1;
                        };
                        if ($131 instanceof Prelude.GT) {
                            return -1;
                        };
                        throw new Error("Failed pattern match at Data.Generic line 289, column 32 - line 293, column 26: " + [ $131.constructor.name ]);
                    };
                    if ($130 instanceof Prelude.LT) {
                        return 1;
                    };
                    if ($130 instanceof Prelude.GT) {
                        return -1;
                    };
                    throw new Error("Failed pattern match at Data.Generic line 288, column 24 - line 295, column 5: " + [ $130.constructor.name ]);
                };
            };
            return Prelude.compare(Prelude.ordInt)(0)($foreign.zipCompare(go)(v.value0)(v1.value0));
        };
        if (v instanceof SRecord) {
            return Prelude.LT.value;
        };
        if (v1 instanceof SRecord) {
            return Prelude.GT.value;
        };
        if (v instanceof SInt && v1 instanceof SInt) {
            return Prelude.compare(Prelude.ordInt)(v.value0)(v1.value0);
        };
        if (v instanceof SInt) {
            return Prelude.LT.value;
        };
        if (v1 instanceof SInt) {
            return Prelude.GT.value;
        };
        if (v instanceof SBoolean && v1 instanceof SBoolean) {
            return Prelude.compare(Prelude.ordBoolean)(v.value0)(v1.value0);
        };
        if (v instanceof SBoolean) {
            return Prelude.LT.value;
        };
        if (v1 instanceof SBoolean) {
            return Prelude.GT.value;
        };
        if (v instanceof SNumber && v1 instanceof SNumber) {
            return Prelude.compare(Prelude.ordNumber)(v.value0)(v1.value0);
        };
        if (v instanceof SNumber) {
            return Prelude.LT.value;
        };
        if (v1 instanceof SNumber) {
            return Prelude.GT.value;
        };
        if (v instanceof SString && v1 instanceof SString) {
            return Prelude.compare(Prelude.ordString)(v.value0)(v1.value0);
        };
        if (v instanceof SString) {
            return Prelude.LT.value;
        };
        if (v1 instanceof SString) {
            return Prelude.GT.value;
        };
        if (v instanceof SChar && v1 instanceof SChar) {
            return Prelude.compare(Prelude.ordChar)(v.value0)(v1.value0);
        };
        if (v instanceof SChar) {
            return Prelude.LT.value;
        };
        if (v1 instanceof SChar) {
            return Prelude.GT.value;
        };
        if (v instanceof SArray && v1 instanceof SArray) {
            return Prelude.compare(Prelude.ordInt)(0)($foreign.zipCompare(function (x) {
                return function (y) {
                    var $156 = Prelude.compare(ordGeneric)(x(Prelude.unit))(y(Prelude.unit));
                    if ($156 instanceof Prelude.EQ) {
                        return 0;
                    };
                    if ($156 instanceof Prelude.LT) {
                        return 1;
                    };
                    if ($156 instanceof Prelude.GT) {
                        return -1;
                    };
                    throw new Error("Failed pattern match at Data.Generic line 312, column 71 - line 315, column 81: " + [ $156.constructor.name ]);
                };
            })(v.value0)(v1.value0));
        };
        throw new Error("Failed pattern match at Data.Generic line 278, column 5 - line 285, column 5: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var gCompare = function (dictGeneric) {
    return function (x) {
        return function (y) {
            return Prelude.compare(ordGeneric)(toSpine(dictGeneric)(x))(toSpine(dictGeneric)(y));
        };
    };
};
var anyProxy = (Type_Proxy["Proxy"]).value;
var genericArray = function (dictGeneric) {
    return new Generic(function (v) {
        if (v instanceof SArray) {
            return Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(function ($197) {
                return fromSpine(dictGeneric)((function (v1) {
                    return v1(Prelude.unit);
                })($197));
            })(v.value0);
        };
        return Data_Maybe.Nothing.value;
    }, function (x) {
        var lowerProxy = function (v) {
            return anyProxy;
        };
        return new SigArray(function (unit) {
            return toSignature(dictGeneric)(lowerProxy(x));
        });
    }, function (xs) {
        return new SArray(Prelude["<$>"](Prelude.functorArray)(function (x) {
            return function (y) {
                return toSpine(dictGeneric)(x);
            };
        })(xs));
    });
};
var genericEither = function (dictGeneric) {
    return function (dictGeneric1) {
        return new Generic(function (v) {
            if (v instanceof SProd && (v.value0 === "Data.Either.Left" && v.value1.length === 1)) {
                return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Either.Left.create)(fromSpine(dictGeneric)(v.value1[0](Prelude.unit)));
            };
            if (v instanceof SProd && (v.value0 === "Data.Either.Right" && v.value1.length === 1)) {
                return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Either.Right.create)(fromSpine(dictGeneric1)(v.value1[0](Prelude.unit)));
            };
            return Data_Maybe.Nothing.value;
        }, function (x) {
            var rproxy = function (v) {
                return anyProxy;
            };
            var lproxy = function (v) {
                return anyProxy;
            };
            return new SigProd("Data.Either.Either", [ {
                sigConstructor: "Data.Either.Left", 
                sigValues: [ function (u) {
                    return toSignature(dictGeneric)(lproxy(x));
                } ]
            }, {
                sigConstructor: "Data.Either.Right", 
                sigValues: [ function (u) {
                    return toSignature(dictGeneric1)(rproxy(x));
                } ]
            } ]);
        }, function (v) {
            if (v instanceof Data_Either.Left) {
                return new SProd("Data.Either.Left", [ function (u) {
                    return toSpine(dictGeneric)(v.value0);
                } ]);
            };
            if (v instanceof Data_Either.Right) {
                return new SProd("Data.Either.Right", [ function (u) {
                    return toSpine(dictGeneric1)(v.value0);
                } ]);
            };
            throw new Error("Failed pattern match at Data.Generic line 215, column 5 - line 216, column 5: " + [ v.constructor.name ]);
        });
    };
};
var genericMaybe = function (dictGeneric) {
    return new Generic(function (v) {
        if (v instanceof SProd && (v.value0 === "Data.Maybe.Just" && v.value1.length === 1)) {
            return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Maybe.Just.create)(fromSpine(dictGeneric)(v.value1[0](Prelude.unit)));
        };
        if (v instanceof SProd && (v.value0 === "Data.Maybe.Nothing" && v.value1.length === 0)) {
            return Prelude["return"](Data_Maybe.applicativeMaybe)(Data_Maybe.Nothing.value);
        };
        return Data_Maybe.Nothing.value;
    }, function (x) {
        var mbProxy = function (v) {
            return anyProxy;
        };
        return new SigProd("Data.Maybe.Maybe", [ {
            sigConstructor: "Data.Maybe.Just", 
            sigValues: [ function (u) {
                return toSignature(dictGeneric)(mbProxy(x));
            } ]
        }, {
            sigConstructor: "Data.Maybe.Nothing", 
            sigValues: [  ]
        } ]);
    }, function (v) {
        if (v instanceof Data_Maybe.Just) {
            return new SProd("Data.Maybe.Just", [ function (u) {
                return toSpine(dictGeneric)(v.value0);
            } ]);
        };
        if (v instanceof Data_Maybe.Nothing) {
            return new SProd("Data.Maybe.Nothing", [  ]);
        };
        throw new Error("Failed pattern match at Data.Generic line 198, column 7 - line 199, column 7: " + [ v.constructor.name ]);
    });
};
var genericTuple = function (dictGeneric) {
    return function (dictGeneric1) {
        return new Generic(function (v) {
            if (v instanceof SProd && (v.value0 === "Data.Tuple.Tuple" && v.value1.length === 2)) {
                return Prelude["<*>"](Data_Maybe.applyMaybe)(Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Tuple.Tuple.create)(fromSpine(dictGeneric)(v.value1[0](Prelude.unit))))(fromSpine(dictGeneric1)(v.value1[1](Prelude.unit)));
            };
            return Data_Maybe.Nothing.value;
        }, function (x) {
            var sndProxy = function (v) {
                return anyProxy;
            };
            var fstProxy = function (v) {
                return anyProxy;
            };
            return new SigProd("Data.Tuple.Tuple", [ {
                sigConstructor: "Data.Tuple.Tuple", 
                sigValues: [ function (u) {
                    return toSignature(dictGeneric)(fstProxy(x));
                }, function (u) {
                    return toSignature(dictGeneric1)(sndProxy(x));
                } ]
            } ]);
        }, function (v) {
            return new SProd("Data.Tuple.Tuple", [ function (u) {
                return toSpine(dictGeneric)(v.value0);
            }, function (u) {
                return toSpine(dictGeneric1)(v.value1);
            } ]);
        });
    };
};
module.exports = {
    SigProd: SigProd, 
    SigRecord: SigRecord, 
    SigNumber: SigNumber, 
    SigBoolean: SigBoolean, 
    SigInt: SigInt, 
    SigString: SigString, 
    SigChar: SigChar, 
    SigArray: SigArray, 
    SProd: SProd, 
    SRecord: SRecord, 
    SNumber: SNumber, 
    SBoolean: SBoolean, 
    SInt: SInt, 
    SString: SString, 
    SChar: SChar, 
    SArray: SArray, 
    Generic: Generic, 
    gCompare: gCompare, 
    gEq: gEq, 
    gShow: gShow, 
    isValidSpine: isValidSpine, 
    showDataConstructor: showDataConstructor, 
    showSignature: showSignature, 
    fromSpine: fromSpine, 
    toSignature: toSignature, 
    toSpine: toSpine, 
    showGenericSignature: showGenericSignature, 
    genericNumber: genericNumber, 
    genericInt: genericInt, 
    genericString: genericString, 
    genericChar: genericChar, 
    genericBool: genericBool, 
    genericArray: genericArray, 
    genericTuple: genericTuple, 
    genericMaybe: genericMaybe, 
    genericEither: genericEither, 
    eqGeneric: eqGeneric, 
    ordGeneric: ordGeneric
};

},{"../Data.Array":102,"../Data.Either":114,"../Data.Foldable":120,"../Data.Maybe":152,"../Data.Monoid":159,"../Data.String":168,"../Data.Traversable":170,"../Data.Tuple":171,"../Prelude":203,"../Type.Proxy":204,"./foreign":135}],137:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Traversable = require("../Data.Traversable");
var Identity = function (x) {
    return x;
};
var showIdentity = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Identity (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semiringIdentity = function (dictSemiring) {
    return new Prelude.Semiring(function (v) {
        return function (v1) {
            return Prelude["+"](dictSemiring)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Prelude["*"](dictSemiring)(v)(v1);
        };
    }, Prelude.one(dictSemiring), Prelude.zero(dictSemiring));
};
var semigroupIdenity = function (dictSemigroup) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude["<>"](dictSemigroup)(v)(v1);
        };
    });
};
var runIdentity = function (v) {
    return v;
};
var ringIdentity = function (dictRing) {
    return new Prelude.Ring(function () {
        return semiringIdentity(dictRing["__superclass_Prelude.Semiring_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude["-"](dictRing)(v)(v1);
        };
    });
};
var monoidIdentity = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupIdenity(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Data_Monoid.mempty(dictMonoid));
};
var moduloSemiringIdentity = function (dictModuloSemiring) {
    return new Prelude.ModuloSemiring(function () {
        return semiringIdentity(dictModuloSemiring["__superclass_Prelude.Semiring_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude["/"](dictModuloSemiring)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Prelude.mod(dictModuloSemiring)(v)(v1);
        };
    });
};
var functorIdentity = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var invariantIdentity = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorIdentity));
var foldableIdentity = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var traversableIdentity = new Data_Traversable.Traversable(function () {
    return foldableIdentity;
}, function () {
    return functorIdentity;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Identity)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Identity)(f(v));
        };
    };
});
var extendIdentity = new Control_Extend.Extend(function () {
    return functorIdentity;
}, function (f) {
    return function (m) {
        return f(m);
    };
});
var eqIdentity = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordIdentity = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqIdentity(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var divisionRingIdentity = function (dictDivisionRing) {
    return new Prelude.DivisionRing(function () {
        return moduloSemiringIdentity(dictDivisionRing["__superclass_Prelude.ModuloSemiring_1"]());
    }, function () {
        return ringIdentity(dictDivisionRing["__superclass_Prelude.Ring_0"]());
    });
};
var numIdentity = function (dictNum) {
    return new Prelude.Num(function () {
        return divisionRingIdentity(dictNum["__superclass_Prelude.DivisionRing_0"]());
    });
};
var comonadIdentity = new Control_Comonad.Comonad(function () {
    return extendIdentity;
}, function (v) {
    return v;
});
var boundedIdentity = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(dictBounded), Prelude.top(dictBounded));
};
var boundedOrdIdentity = function (dictBoundedOrd) {
    return new Prelude.BoundedOrd(function () {
        return boundedIdentity(dictBoundedOrd["__superclass_Prelude.Bounded_0"]());
    }, function () {
        return ordIdentity(dictBoundedOrd["__superclass_Prelude.Ord_1"]());
    });
};
var booleanAlgebraIdentity = function (dictBooleanAlgebra) {
    return new Prelude.BooleanAlgebra(function () {
        return boundedIdentity(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.conj(dictBooleanAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Prelude.disj(dictBooleanAlgebra)(v)(v1);
        };
    }, function (v) {
        return Prelude.not(dictBooleanAlgebra)(v);
    });
};
var applyIdentity = new Prelude.Apply(function () {
    return functorIdentity;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindIdentity = new Prelude.Bind(function () {
    return applyIdentity;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeIdentity = new Prelude.Applicative(function () {
    return applyIdentity;
}, Identity);
var monadIdentity = new Prelude.Monad(function () {
    return applicativeIdentity;
}, function () {
    return bindIdentity;
});
module.exports = {
    Identity: Identity, 
    runIdentity: runIdentity, 
    eqIdentity: eqIdentity, 
    ordIdentity: ordIdentity, 
    boundedIdentity: boundedIdentity, 
    boundedOrdIdentity: boundedOrdIdentity, 
    booleanAlgebraIdentity: booleanAlgebraIdentity, 
    semigroupIdenity: semigroupIdenity, 
    monoidIdentity: monoidIdentity, 
    semiringIdentity: semiringIdentity, 
    moduloSemiringIdentity: moduloSemiringIdentity, 
    ringIdentity: ringIdentity, 
    divisionRingIdentity: divisionRingIdentity, 
    numIdentity: numIdentity, 
    showIdentity: showIdentity, 
    functorIdentity: functorIdentity, 
    invariantIdentity: invariantIdentity, 
    applyIdentity: applyIdentity, 
    applicativeIdentity: applicativeIdentity, 
    bindIdentity: bindIdentity, 
    monadIdentity: monadIdentity, 
    extendIdentity: extendIdentity, 
    comonadIdentity: comonadIdentity, 
    foldableIdentity: foldableIdentity, 
    traversableIdentity: traversableIdentity
};

},{"../Control.Comonad":32,"../Control.Extend":36,"../Data.Foldable":120,"../Data.Functor.Invariant":133,"../Data.Monoid":159,"../Data.Traversable":170,"../Prelude":203}],138:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Functor_Coproduct = require("../Data.Functor.Coproduct");
var Data_Maybe = require("../Data.Maybe");
var Inject = function (inj, prj) {
    this.inj = inj;
    this.prj = prj;
};
var prj = function (dict) {
    return dict.prj;
};
var injectReflexive = new Inject(Prelude.id(Prelude.categoryFn), Data_Maybe.Just.create);
var injectLeft = new Inject(function ($1) {
    return Data_Functor_Coproduct.Coproduct(Data_Either.Left.create($1));
}, Data_Functor_Coproduct.coproduct(Data_Maybe.Just.create)(Prelude["const"](Data_Maybe.Nothing.value)));
var inj = function (dict) {
    return dict.inj;
};
var injectRight = function (dictInject) {
    return new Inject(function ($2) {
        return Data_Functor_Coproduct.Coproduct(Data_Either.Right.create(inj(dictInject)($2)));
    }, Data_Functor_Coproduct.coproduct(Prelude["const"](Data_Maybe.Nothing.value))(prj(dictInject)));
};
module.exports = {
    Inject: Inject, 
    prj: prj, 
    inj: inj, 
    injectReflexive: injectReflexive, 
    injectLeft: injectLeft, 
    injectRight: injectRight
};

},{"../Data.Either":114,"../Data.Functor.Coproduct":132,"../Data.Maybe":152,"../Prelude":203}],139:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Const = require("../Data.Const");
var Data_Either = require("../Data.Either");
var Data_Functor_Coproduct = require("../Data.Functor.Coproduct");
var Data_Identity = require("../Data.Identity");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_First = require("../Data.Maybe.First");
var Data_Profunctor = require("../Data.Profunctor");
var Data_Profunctor_Choice_1 = require("../Data.Profunctor.Choice");
var Data_Profunctor_Choice_1 = require("../Data.Profunctor.Choice");
var Tagged = function (x) {
    return x;
};
var unTagged = function (v) {
    return v;
};
var profunctorTagged = new Data_Profunctor.Profunctor(function (v) {
    return function (f) {
        return function (v1) {
            return f(v1);
        };
    };
});
var prj = function (p) {
    return function ($25) {
        return Data_Maybe_First.runFirst(Data_Const.getConst(p(Data_Profunctor_Choice_1.choiceFn)(Data_Const.applicativeConst(Data_Maybe_First.monoidFirst))(function ($26) {
            return Data_Const.Const(Data_Maybe_First.First(Data_Maybe.Just.create($26)));
        })($25)));
    };
};
var prism = function (f) {
    return function (g) {
        return function (dictChoice) {
            return function (dictApplicative) {
                return function ($27) {
                    return Data_Profunctor.dimap(dictChoice["__superclass_Data.Profunctor.Profunctor_0"]())(g)(Data_Either.either(Prelude.pure(dictApplicative))(Prelude.map((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(f)))(Data_Profunctor_Choice_1.right(dictChoice)($27));
                };
            };
        };
    };
};
var prism$prime = function (f) {
    return function (g) {
        return function (dictChoice) {
            return function (dictApplicative) {
                return prism(f)(function (s) {
                    return Data_Maybe.maybe(new Data_Either.Left(s))(Data_Either.Right.create)(g(s));
                })(dictChoice)(dictApplicative);
            };
        };
    };
};
var injRE = function (dictChoice) {
    return function (dictApplicative) {
        return prism$prime(Data_Either.Right.create)(Data_Either.either(Prelude["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create))(dictChoice)(dictApplicative);
    };
};
var injRC = function (dictChoice) {
    return function (dictApplicative) {
        return prism$prime(Data_Functor_Coproduct.right)(Data_Functor_Coproduct.coproduct(Prelude["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create))(dictChoice)(dictApplicative);
    };
};
var injLE = function (dictChoice) {
    return function (dictApplicative) {
        return prism$prime(Data_Either.Left.create)(Data_Either.either(Data_Maybe.Just.create)(Prelude["const"](Data_Maybe.Nothing.value)))(dictChoice)(dictApplicative);
    };
};
var injLC = function (dictChoice) {
    return function (dictApplicative) {
        return prism$prime(Data_Functor_Coproduct.left)(Data_Functor_Coproduct.coproduct(Data_Maybe.Just.create)(Prelude["const"](Data_Maybe.Nothing.value)))(dictChoice)(dictApplicative);
    };
};
var injI = function (dictChoice) {
    return function (dictApplicative) {
        return prism$prime(Prelude.id(Prelude.categoryFn))(Data_Maybe.Just.create)(dictChoice)(dictApplicative);
    };
};
var choiceTagged = new Data_Profunctor_Choice_1.Choice(function () {
    return profunctorTagged;
}, function (v) {
    return new Data_Either.Left(v);
}, function (v) {
    return new Data_Either.Right(v);
});
var inj = function (p) {
    return function ($28) {
        return Data_Identity.runIdentity(unTagged(p(choiceTagged)(Data_Identity.applicativeIdentity)(Tagged(Data_Identity.Identity($28)))));
    };
};
module.exports = {
    injRC: injRC, 
    injRE: injRE, 
    injLC: injLC, 
    injLE: injLE, 
    injI: injI, 
    prj: prj, 
    inj: inj
};

},{"../Data.Const":110,"../Data.Either":114,"../Data.Functor.Coproduct":132,"../Data.Identity":137,"../Data.Maybe":152,"../Data.Maybe.First":148,"../Data.Profunctor":164,"../Data.Profunctor.Choice":163,"../Prelude":203}],140:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Int.Bits

exports.andImpl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 & n2;
  };
};

exports.orImpl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 | n2;
  };
};

exports.xorImpl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 ^ n2;
  };
};

exports.shl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 << n2;
  };
};

exports.shr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 >> n2;
  };
};

exports.zshr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 >>> n2;
  };
};

exports.complement = function (n) {
  /* jshint bitwise: false */
  return ~n;
};

},{}],141:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var $dot$bar$dot = $foreign.orImpl;
var $dot$up$dot = $foreign.xorImpl;
var $dot$amp$dot = $foreign.andImpl;
module.exports = {
    ".^.": $dot$up$dot, 
    ".|.": $dot$bar$dot, 
    ".&.": $dot$amp$dot, 
    complement: $foreign.complement, 
    zshr: $foreign.zshr, 
    shr: $foreign.shr, 
    shl: $foreign.shl
};

},{"./foreign":140}],142:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Int

exports.fromNumberImpl = function (just) {
  return function (nothing) {
    return function (n) {
      /* jshint bitwise: false */
      return (n | 0) === n ? just(n) : nothing;
    };
  };
};

exports.toNumber = function (n) {
  return n;
};

exports.fromStringImpl = function (just) {
  return function (nothing) {
    return function (s) {
      /* jshint bitwise: false */
      var i = parseFloat(s);
      return (i | 0) === i ? just(i) : nothing;
    };
  };
};

},{}],143:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Int_Bits = require("../Data.Int.Bits");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_Unsafe = require("../Data.Maybe.Unsafe");
var $$Math = require("../Math");
var odd = function (x) {
    return (x & 1) !== 0;
};
var fromString = $foreign.fromStringImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var fromNumber = $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unsafeClamp = function (x) {
    if (x >= $foreign.toNumber(Prelude.top(Prelude.boundedInt))) {
        return Prelude.top(Prelude.boundedInt);
    };
    if (x <= $foreign.toNumber(Prelude.bottom(Prelude.boundedInt))) {
        return Prelude.bottom(Prelude.boundedInt);
    };
    if (Prelude.otherwise) {
        return Data_Maybe_Unsafe.fromJust(fromNumber(x));
    };
    throw new Error("Failed pattern match at Data.Int line 49, column 1 - line 56, column 1: " + [ x.constructor.name ]);
};
var round = function ($1) {
    return unsafeClamp($$Math.round($1));
};
var floor = function ($2) {
    return unsafeClamp($$Math.floor($2));
};
var even = function (x) {
    return (x & 1) === 0;
};
var ceil = function ($3) {
    return unsafeClamp($$Math.ceil($3));
};
module.exports = {
    odd: odd, 
    even: even, 
    fromString: fromString, 
    round: round, 
    floor: floor, 
    ceil: ceil, 
    fromNumber: fromNumber, 
    toNumber: $foreign.toNumber
};

},{"../Data.Int.Bits":141,"../Data.Maybe":152,"../Data.Maybe.Unsafe":151,"../Math":201,"../Prelude":203,"./foreign":142}],144:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Lazy

exports.defer = function () {

  function Defer (thunk) {
    if (this instanceof Defer) {
      this.thunk = thunk;
      return this;
    } else {
      return new Defer(thunk);
    }
  }

  Defer.prototype.force = function () {
    var value = this.thunk();
    delete this.thunk;
    this.force = function () {
      return value;
    };
    return value;
  };

  return Defer;

}();

exports.force = function (l) {
  return l.force();
};

},{}],145:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Data_Monoid = require("../Data.Monoid");
var Control_Lazy = require("../Control.Lazy");
var showLazy = function (dictShow) {
    return new Prelude.Show(function (x) {
        return "Lazy " + Prelude.show(dictShow)($foreign.force(x));
    });
};
var semiringLazy = function (dictSemiring) {
    return new Prelude.Semiring(function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude["+"](dictSemiring)($foreign.force(a))($foreign.force(b));
            });
        };
    }, function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude["*"](dictSemiring)($foreign.force(a))($foreign.force(b));
            });
        };
    }, $foreign.defer(function (v) {
        return Prelude.one(dictSemiring);
    }), $foreign.defer(function (v) {
        return Prelude.zero(dictSemiring);
    }));
};
var semigroupLazy = function (dictSemigroup) {
    return new Prelude.Semigroup(function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude["<>"](dictSemigroup)($foreign.force(a))($foreign.force(b));
            });
        };
    });
};
var ringLazy = function (dictRing) {
    return new Prelude.Ring(function () {
        return semiringLazy(dictRing["__superclass_Prelude.Semiring_0"]());
    }, function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude["-"](dictRing)($foreign.force(a))($foreign.force(b));
            });
        };
    });
};
var monoidLazy = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupLazy(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, $foreign.defer(function (v) {
        return Data_Monoid.mempty(dictMonoid);
    }));
};
var moduloSemiringLazy = function (dictModuloSemiring) {
    return new Prelude.ModuloSemiring(function () {
        return semiringLazy(dictModuloSemiring["__superclass_Prelude.Semiring_0"]());
    }, function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude["/"](dictModuloSemiring)($foreign.force(a))($foreign.force(b));
            });
        };
    }, function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude.mod(dictModuloSemiring)($foreign.force(a))($foreign.force(b));
            });
        };
    });
};
var lazyLazy = new Control_Lazy.Lazy(function (f) {
    return $foreign.defer(function (v) {
        return $foreign.force(f(Prelude.unit));
    });
});
var functorLazy = new Prelude.Functor(function (f) {
    return function (l) {
        return $foreign.defer(function (v) {
            return f($foreign.force(l));
        });
    };
});
var extendLazy = new Control_Extend.Extend(function () {
    return functorLazy;
}, function (f) {
    return function (x) {
        return $foreign.defer(function (v) {
            return f(x);
        });
    };
});
var eqLazy = function (dictEq) {
    return new Prelude.Eq(function (x) {
        return function (y) {
            return Prelude["=="](dictEq)($foreign.force(x))($foreign.force(y));
        };
    });
};
var ordLazy = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqLazy(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (x) {
        return function (y) {
            return Prelude.compare(dictOrd)($foreign.force(x))($foreign.force(y));
        };
    });
};
var divisionRingLazy = function (dictDivisionRing) {
    return new Prelude.DivisionRing(function () {
        return moduloSemiringLazy(dictDivisionRing["__superclass_Prelude.ModuloSemiring_1"]());
    }, function () {
        return ringLazy(dictDivisionRing["__superclass_Prelude.Ring_0"]());
    });
};
var numLazy = function (dictNum) {
    return new Prelude.Num(function () {
        return divisionRingLazy(dictNum["__superclass_Prelude.DivisionRing_0"]());
    });
};
var comonadLazy = new Control_Comonad.Comonad(function () {
    return extendLazy;
}, $foreign.force);
var boundedLazy = function (dictBounded) {
    return new Prelude.Bounded($foreign.defer(function (v) {
        return Prelude.bottom(dictBounded);
    }), $foreign.defer(function (v) {
        return Prelude.top(dictBounded);
    }));
};
var boundedOrdLazy = function (dictBoundedOrd) {
    return new Prelude.BoundedOrd(function () {
        return boundedLazy(dictBoundedOrd["__superclass_Prelude.Bounded_0"]());
    }, function () {
        return ordLazy(dictBoundedOrd["__superclass_Prelude.Ord_1"]());
    });
};
var applyLazy = new Prelude.Apply(function () {
    return functorLazy;
}, function (f) {
    return function (x) {
        return $foreign.defer(function (v) {
            return $foreign.force(f)($foreign.force(x));
        });
    };
});
var bindLazy = new Prelude.Bind(function () {
    return applyLazy;
}, function (l) {
    return function (f) {
        return $foreign.defer(function (v) {
            return $foreign.force(f($foreign.force(l)));
        });
    };
});
var booleanAlgebraLazy = function (dictBooleanAlgebra) {
    return new Prelude.BooleanAlgebra(function () {
        return boundedLazy(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]());
    }, function (a) {
        return function (b) {
            return Prelude["<*>"](applyLazy)(Prelude["<$>"](functorLazy)(Prelude.conj(dictBooleanAlgebra))(a))(b);
        };
    }, function (a) {
        return function (b) {
            return Prelude["<*>"](applyLazy)(Prelude["<$>"](functorLazy)(Prelude.disj(dictBooleanAlgebra))(a))(b);
        };
    }, function (a) {
        return Prelude["<$>"](functorLazy)(Prelude.not(dictBooleanAlgebra))(a);
    });
};
var applicativeLazy = new Prelude.Applicative(function () {
    return applyLazy;
}, function (a) {
    return $foreign.defer(function (v) {
        return a;
    });
});
var monadLazy = new Prelude.Monad(function () {
    return applicativeLazy;
}, function () {
    return bindLazy;
});
module.exports = {
    semiringLazy: semiringLazy, 
    ringLazy: ringLazy, 
    moduloSemiringLazy: moduloSemiringLazy, 
    divisionRingLazy: divisionRingLazy, 
    numLazy: numLazy, 
    eqLazy: eqLazy, 
    ordLazy: ordLazy, 
    boundedLazy: boundedLazy, 
    boundedOrdLazy: boundedOrdLazy, 
    semigroupLazy: semigroupLazy, 
    monoidLazy: monoidLazy, 
    booleanAlgebraLazy: booleanAlgebraLazy, 
    functorLazy: functorLazy, 
    applyLazy: applyLazy, 
    applicativeLazy: applicativeLazy, 
    bindLazy: bindLazy, 
    monadLazy: monadLazy, 
    extendLazy: extendLazy, 
    comonadLazy: comonadLazy, 
    showLazy: showLazy, 
    lazyLazy: lazyLazy, 
    force: $foreign.force, 
    defer: $foreign.defer
};

},{"../Control.Comonad":32,"../Control.Extend":36,"../Control.Lazy":37,"../Data.Monoid":159,"../Prelude":203,"./foreign":144}],146:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Lazy = require("../Control.Lazy");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_Plus = require("../Control.Plus");
var Data_Foldable = require("../Data.Foldable");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Nil = (function () {
    function Nil() {

    };
    Nil.value = new Nil();
    return Nil;
})();
var Cons = (function () {
    function Cons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Cons.create = function (value0) {
        return function (value1) {
            return new Cons(value0, value1);
        };
    };
    return Cons;
})();
var $colon = Cons.create;
var updateAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0 && v2 instanceof Cons) {
                return new Data_Maybe.Just(new Cons(v1, v2.value1));
            };
            if (v2 instanceof Cons) {
                return Prelude["<$>"](Data_Maybe.functorMaybe)(Cons.create(v2.value0))(updateAt(v - 1)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var uncons = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Cons) {
        return new Data_Maybe.Just({
            head: v.value0, 
            tail: v.value1
        });
    };
    throw new Error("Failed pattern match at Data.List line 273, column 1 - line 274, column 1: " + [ v.constructor.name ]);
};
var toUnfoldable = function (dictUnfoldable) {
    return Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
        return Prelude["<$>"](Data_Maybe.functorMaybe)(function (rec) {
            return new Data_Tuple.Tuple(rec.head, rec.tail);
        })(uncons(xs));
    });
};
var tail = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Cons) {
        return new Data_Maybe.Just(v.value1);
    };
    throw new Error("Failed pattern match at Data.List line 254, column 1 - line 255, column 1: " + [ v.constructor.name ]);
};
var span = function (v) {
    return function (v1) {
        if (v1 instanceof Cons && v(v1.value0)) {
            var $134 = span(v)(v1.value1);
            return {
                init: new Cons(v1.value0, $134.init), 
                rest: $134.rest
            };
        };
        return {
            init: Nil.value, 
            rest: v1
        };
    };
};
var singleton = function (a) {
    return new Cons(a, Nil.value);
};
var sortBy = function (cmp) {
    var merge = function (v) {
        return function (v1) {
            if (v instanceof Cons && v1 instanceof Cons) {
                if (Prelude["=="](Prelude.eqOrdering)(cmp(v.value0)(v1.value0))(Prelude.GT.value)) {
                    return new Cons(v1.value0, merge(v)(v1.value1));
                };
                if (Prelude.otherwise) {
                    return new Cons(v.value0, merge(v.value1)(v1));
                };
            };
            if (v instanceof Nil) {
                return v1;
            };
            if (v1 instanceof Nil) {
                return v;
            };
            throw new Error("Failed pattern match at Data.List line 475, column 3 - line 478, column 3: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
    var mergePairs = function (v) {
        if (v instanceof Cons && v.value1 instanceof Cons) {
            return new Cons(merge(v.value0)(v.value1.value0), mergePairs(v.value1.value1));
        };
        return v;
    };
    var mergeAll = function (__copy_v) {
        var v = __copy_v;
        tco: while (true) {
            if (v instanceof Cons && v.value1 instanceof Nil) {
                return v.value0;
            };
            var __tco_v = mergePairs(v);
            v = __tco_v;
            continue tco;
        };
    };
    var sequences = function (v) {
        if (v instanceof Cons && v.value1 instanceof Cons) {
            if (Prelude["=="](Prelude.eqOrdering)(cmp(v.value0)(v.value1.value0))(Prelude.GT.value)) {
                return descending(v.value1.value0)(singleton(v.value0))(v.value1.value1);
            };
            if (Prelude.otherwise) {
                return ascending(v.value1.value0)(Cons.create(v.value0))(v.value1.value1);
            };
        };
        return singleton(v);
    };
    var descending = function (__copy_a) {
        return function (__copy_as) {
            return function (__copy_v) {
                var a = __copy_a;
                var as = __copy_as;
                var v = __copy_v;
                tco: while (true) {
                    if (v instanceof Cons && Prelude["=="](Prelude.eqOrdering)(cmp(a)(v.value0))(Prelude.GT.value)) {
                        var __tco_a = v.value0;
                        var __tco_as = new Cons(a, as);
                        var __tco_v = v.value1;
                        a = __tco_a;
                        as = __tco_as;
                        v = __tco_v;
                        continue tco;
                    };
                    return new Cons(new Cons(a, as), sequences(v));
                };
            };
        };
    };
    var ascending = function (a) {
        return function (as) {
            return function (v) {
                if (v instanceof Cons && Prelude["/="](Prelude.eqOrdering)(cmp(a)(v.value0))(Prelude.GT.value)) {
                    return ascending(v.value0)(function (ys) {
                        return as(new Cons(a, ys));
                    })(v.value1);
                };
                return new Cons(as(singleton(a)), sequences(v));
            };
        };
    };
    return function ($372) {
        return mergeAll(sequences($372));
    };
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Prelude.compare(dictOrd))(xs);
    };
};
var showList = function (dictShow) {
    return new Prelude.Show(function (v) {
        if (v instanceof Nil) {
            return "Nil";
        };
        if (v instanceof Cons) {
            return "Cons (" + (Prelude.show(dictShow)(v.value0) + (") (" + (Prelude.show(showList(dictShow))(v.value1) + ")")));
        };
        throw new Error("Failed pattern match at Data.List line 727, column 3 - line 728, column 3: " + [ v.constructor.name ]);
    });
};
var semigroupList = new Prelude.Semigroup(function (v) {
    return function (ys) {
        if (v instanceof Nil) {
            return ys;
        };
        if (v instanceof Cons) {
            return new Cons(v.value0, Prelude["<>"](semigroupList)(v.value1)(ys));
        };
        throw new Error("Failed pattern match at Data.List line 751, column 3 - line 752, column 3: " + [ v.constructor.name, ys.constructor.name ]);
    };
});
var reverse = (function () {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Nil) {
                    return acc;
                };
                if (v instanceof Cons) {
                    var __tco_acc = new Cons(v.value0, acc);
                    var __tco_v = v.value1;
                    acc = __tco_acc;
                    v = __tco_v;
                    continue tco;
                };
                throw new Error("Failed pattern match at Data.List line 371, column 1 - line 379, column 1: " + [ acc.constructor.name, v.constructor.name ]);
            };
        };
    };
    return go(Nil.value);
})();
var snoc = function (xs) {
    return function (x) {
        return reverse(new Cons(x, reverse(xs)));
    };
};
var take = (function () {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            return function (__copy_v1) {
                var acc = __copy_acc;
                var v = __copy_v;
                var v1 = __copy_v1;
                tco: while (true) {
                    if (v === 0) {
                        return reverse(acc);
                    };
                    if (v1 instanceof Nil) {
                        return reverse(acc);
                    };
                    if (v1 instanceof Cons) {
                        var __tco_acc = new Cons(v1.value0, acc);
                        var __tco_v = v - 1;
                        var __tco_v1 = v1.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 493, column 1 - line 502, column 1: " + [ acc.constructor.name, v.constructor.name, v1.constructor.name ]);
                };
            };
        };
    };
    return go(Nil.value);
})();
var takeWhile = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Cons && p(v.value0)) {
                    var __tco_acc = new Cons(v.value0, acc);
                    var __tco_v = v.value1;
                    acc = __tco_acc;
                    v = __tco_v;
                    continue tco;
                };
                return reverse(acc);
            };
        };
    };
    return go(Nil.value);
};
var unfoldableList = new Data_Unfoldable.Unfoldable(function (f) {
    return function (b) {
        var go = function (__copy_source) {
            return function (__copy_memo) {
                var source = __copy_source;
                var memo = __copy_memo;
                tco: while (true) {
                    var $188 = f(source);
                    if ($188 instanceof Data_Maybe.Nothing) {
                        return reverse(memo);
                    };
                    if ($188 instanceof Data_Maybe.Just) {
                        var __tco_memo = new Cons($188.value0.value0, memo);
                        source = $188.value0.value1;
                        memo = __tco_memo;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 777, column 24 - line 781, column 1: " + [ $188.constructor.name ]);
                };
            };
        };
        return go(b)(Nil.value);
    };
});
var zipWith = function (f) {
    return function (xs) {
        return function (ys) {
            var go = function (__copy_v) {
                return function (__copy_v1) {
                    return function (__copy_acc) {
                        var v = __copy_v;
                        var v1 = __copy_v1;
                        var acc = __copy_acc;
                        tco: while (true) {
                            if (v instanceof Nil) {
                                return acc;
                            };
                            if (v1 instanceof Nil) {
                                return acc;
                            };
                            if (v instanceof Cons && v1 instanceof Cons) {
                                var __tco_v = v.value1;
                                var __tco_v1 = v1.value1;
                                var __tco_acc = new Cons(f(v.value0)(v1.value0), acc);
                                v = __tco_v;
                                v1 = __tco_v1;
                                acc = __tco_acc;
                                continue tco;
                            };
                            throw new Error("Failed pattern match at Data.List line 657, column 1 - line 665, column 1: " + [ v.constructor.name, v1.constructor.name, acc.constructor.name ]);
                        };
                    };
                };
            };
            return reverse(go(xs)(ys)(Nil.value));
        };
    };
};
var zip = zipWith(Data_Tuple.Tuple.create);
var replicateM = function (dictMonad) {
    return function (n) {
        return function (m) {
            if (n < 1) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Nil.value);
            };
            if (Prelude.otherwise) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(replicateM(dictMonad)(n - 1)(m))(function (v1) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Cons(v, v1));
                    });
                });
            };
            throw new Error("Failed pattern match at Data.List line 158, column 1 - line 167, column 1: " + [ n.constructor.name, m.constructor.name ]);
        };
    };
};
var replicate = function (n) {
    return function (value) {
        var go = function (__copy_n1) {
            return function (__copy_rest) {
                var n1 = __copy_n1;
                var rest = __copy_rest;
                tco: while (true) {
                    if (n1 <= 0) {
                        return rest;
                    };
                    if (Prelude.otherwise) {
                        var __tco_n1 = n1 - 1;
                        var __tco_rest = new Cons(value, rest);
                        n1 = __tco_n1;
                        rest = __tco_rest;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 151, column 1 - line 157, column 1: " + [ n1.constructor.name, rest.constructor.name ]);
                };
            };
        };
        return go(n)(Nil.value);
    };
};
var range = function (start) {
    return function (end) {
        if (start === end) {
            return singleton(start);
        };
        if (Prelude.otherwise) {
            var go = function (__copy_s) {
                return function (__copy_e) {
                    return function (__copy_step) {
                        return function (__copy_rest) {
                            var s = __copy_s;
                            var e = __copy_e;
                            var step = __copy_step;
                            var rest = __copy_rest;
                            tco: while (true) {
                                if (s === e) {
                                    return new Cons(s, rest);
                                };
                                if (Prelude.otherwise) {
                                    var __tco_s = s + step | 0;
                                    var __tco_e = e;
                                    var __tco_step = step;
                                    var __tco_rest = new Cons(s, rest);
                                    s = __tco_s;
                                    e = __tco_e;
                                    step = __tco_step;
                                    rest = __tco_rest;
                                    continue tco;
                                };
                                throw new Error("Failed pattern match at Data.List line 143, column 1 - line 150, column 1: " + [ s.constructor.name, e.constructor.name, step.constructor.name, rest.constructor.name ]);
                            };
                        };
                    };
                };
            };
            return go(end)(start)((function () {
                var $211 = start > end;
                if ($211) {
                    return 1;
                };
                if (!$211) {
                    return -1;
                };
                throw new Error("Failed pattern match at Data.List line 144, column 45 - line 144, column 74: " + [ $211.constructor.name ]);
            })())(Nil.value);
        };
        throw new Error("Failed pattern match at Data.List line 143, column 1 - line 150, column 1: " + [ start.constructor.name, end.constructor.name ]);
    };
};
var $dot$dot = range;
var $$null = function (v) {
    if (v instanceof Nil) {
        return true;
    };
    return false;
};
var monoidList = new Data_Monoid.Monoid(function () {
    return semigroupList;
}, Nil.value);
var mapMaybe = function (f) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Nil) {
                    return reverse(acc);
                };
                if (v instanceof Cons) {
                    var $215 = f(v.value0);
                    if ($215 instanceof Data_Maybe.Nothing) {
                        var __tco_acc = acc;
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                    if ($215 instanceof Data_Maybe.Just) {
                        var __tco_acc = new Cons($215.value0, acc);
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 427, column 5 - line 433, column 1: " + [ $215.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.List line 423, column 1 - line 433, column 1: " + [ acc.constructor.name, v.constructor.name ]);
            };
        };
    };
    return go(Nil.value);
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Prelude["<*>"]((dictAlternative["__superclass_Prelude.Applicative_0"]())["__superclass_Prelude.Apply_0"]())(Prelude["<$>"](((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())["__superclass_Prelude.Functor_0"]())(Cons.create)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt["<|>"]((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())(some(dictAlternative)(dictLazy)(v))(Prelude.pure(dictAlternative["__superclass_Prelude.Applicative_0"]())(Nil.value));
        };
    };
};
var last = function (__copy_v) {
    var v = __copy_v;
    tco: while (true) {
        if (v instanceof Cons && v.value1 instanceof Nil) {
            return new Data_Maybe.Just(v.value0);
        };
        if (v instanceof Cons) {
            var __tco_v = v.value1;
            v = __tco_v;
            continue tco;
        };
        return Data_Maybe.Nothing.value;
    };
};
var insertBy = function (v) {
    return function (x) {
        return function (v1) {
            if (v1 instanceof Nil) {
                return new Cons(x, Nil.value);
            };
            if (v1 instanceof Cons) {
                var $228 = v(x)(v1.value0);
                if ($228 instanceof Prelude.GT) {
                    return new Cons(v1.value0, insertBy(v)(x)(v1.value1));
                };
                return new Cons(x, v1);
            };
            throw new Error("Failed pattern match at Data.List line 225, column 1 - line 226, column 1: " + [ v.constructor.name, x.constructor.name, v1.constructor.name ]);
        };
    };
};
var insertAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0) {
                return new Data_Maybe.Just(new Cons(v1, v2));
            };
            if (v2 instanceof Cons) {
                return Prelude["<$>"](Data_Maybe.functorMaybe)(Cons.create(v2.value0))(insertAt(v - 1)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Prelude.compare(dictOrd));
};
var init = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    var go = function (__copy_v1) {
        return function (__copy_acc) {
            var v1 = __copy_v1;
            var acc = __copy_acc;
            tco: while (true) {
                if (v1 instanceof Cons && v1.value1 instanceof Nil) {
                    return acc;
                };
                if (v1 instanceof Cons) {
                    var __tco_v1 = v1.value1;
                    var __tco_acc = new Cons(v1.value0, acc);
                    v1 = __tco_v1;
                    acc = __tco_acc;
                    continue tco;
                };
                return acc;
            };
        };
    };
    return Data_Maybe.Just.create(reverse(go(v)(Nil.value)));
};
var index = function (__copy_v) {
    return function (__copy_v1) {
        var v = __copy_v;
        var v1 = __copy_v1;
        tco: while (true) {
            if (v instanceof Nil) {
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Cons && v1 === 0) {
                return new Data_Maybe.Just(v.value0);
            };
            if (v instanceof Cons) {
                var __tco_v = v.value1;
                var __tco_v1 = v1 - 1;
                v = __tco_v;
                v1 = __tco_v1;
                continue tco;
            };
            throw new Error("Failed pattern match at Data.List line 284, column 1 - line 285, column 1: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var $bang$bang = index;
var head = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Cons) {
        return new Data_Maybe.Just(v.value0);
    };
    throw new Error("Failed pattern match at Data.List line 239, column 1 - line 240, column 1: " + [ v.constructor.name ]);
};
var transpose = function (v) {
    if (v instanceof Nil) {
        return Nil.value;
    };
    if (v instanceof Cons && v.value0 instanceof Nil) {
        return transpose(v.value1);
    };
    if (v instanceof Cons && v.value0 instanceof Cons) {
        return $colon($colon(v.value0.value0)(mapMaybe(head)(v.value1)))(transpose($colon(v.value0.value1)(mapMaybe(tail)(v.value1))));
    };
    throw new Error("Failed pattern match at Data.List line 694, column 1 - line 695, column 1: " + [ v.constructor.name ]);
};
var groupBy = function (v) {
    return function (v1) {
        if (v1 instanceof Nil) {
            return Nil.value;
        };
        if (v1 instanceof Cons) {
            var $261 = span(v(v1.value0))(v1.value1);
            return new Cons(new Cons(v1.value0, $261.init), groupBy(v)($261.rest));
        };
        throw new Error("Failed pattern match at Data.List line 567, column 1 - line 568, column 1: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var group = function (dictEq) {
    return groupBy(Prelude["=="](dictEq));
};
var group$prime = function (dictOrd) {
    return function ($373) {
        return group(dictOrd["__superclass_Prelude.Eq_0"]())(sort(dictOrd)($373));
    };
};
var functorList = new Prelude.Functor(function (f) {
    return function (lst) {
        var go = function (__copy_v) {
            return function (__copy_acc) {
                var v = __copy_v;
                var acc = __copy_acc;
                tco: while (true) {
                    if (v instanceof Nil) {
                        return acc;
                    };
                    if (v instanceof Cons) {
                        var __tco_v = v.value1;
                        var __tco_acc = new Cons(f(v.value0), acc);
                        v = __tco_v;
                        acc = __tco_acc;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 758, column 3 - line 764, column 1: " + [ v.constructor.name, acc.constructor.name ]);
                };
            };
        };
        return reverse(go(lst)(Nil.value));
    };
});
var fromList = function (dictUnfoldable) {
    return toUnfoldable(dictUnfoldable);
};
var fromFoldable = function (dictFoldable) {
    return Data_Foldable.foldr(dictFoldable)(Cons.create)(Nil.value);
};
var toList = function (dictFoldable) {
    return fromFoldable(dictFoldable);
};
var foldableList = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldl(foldableList)(function (acc) {
            return function ($374) {
                return Prelude.append(dictMonoid["__superclass_Prelude.Semigroup_0"]())(acc)(f($374));
            };
        })(Data_Monoid.mempty(dictMonoid));
    };
}, (function () {
    var go = function (__copy_v) {
        return function (__copy_b) {
            return function (__copy_v1) {
                var v = __copy_v;
                var b = __copy_b;
                var v1 = __copy_v1;
                tco: while (true) {
                    if (v1 instanceof Nil) {
                        return b;
                    };
                    if (v1 instanceof Cons) {
                        var __tco_v = v;
                        var __tco_b = v(b)(v1.value0);
                        var __tco_v1 = v1.value1;
                        v = __tco_v;
                        b = __tco_b;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 767, column 3 - line 771, column 3: " + [ v.constructor.name, b.constructor.name, v1.constructor.name ]);
                };
            };
        };
    };
    return go;
})(), function (v) {
    return function (b) {
        return function (v1) {
            if (v1 instanceof Nil) {
                return b;
            };
            if (v1 instanceof Cons) {
                return v(v1.value0)(Data_Foldable.foldr(foldableList)(v)(b)(v1.value1));
            };
            throw new Error("Failed pattern match at Data.List line 765, column 3 - line 766, column 3: " + [ v.constructor.name, b.constructor.name, v1.constructor.name ]);
        };
    };
});
var length = Data_Foldable.foldl(foldableList)(function (acc) {
    return function (v) {
        return acc + 1 | 0;
    };
})(0);
var traversableList = new Data_Traversable.Traversable(function () {
    return foldableList;
}, function () {
    return functorList;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Nil) {
            return Prelude.pure(dictApplicative)(Nil.value);
        };
        if (v instanceof Cons) {
            return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Cons.create)(v.value0))(Data_Traversable.sequence(traversableList)(dictApplicative)(v.value1));
        };
        throw new Error("Failed pattern match at Data.List line 784, column 3 - line 785, column 3: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Nil) {
                return Prelude.pure(dictApplicative)(Nil.value);
            };
            if (v1 instanceof Cons) {
                return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Cons.create)(v(v1.value0)))(Data_Traversable.traverse(traversableList)(dictApplicative)(v)(v1.value1));
            };
            throw new Error("Failed pattern match at Data.List line 782, column 3 - line 783, column 3: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(traversableList)(dictApplicative)(zipWith(f)(xs)(ys));
            };
        };
    };
};
var unzip = Data_Foldable.foldr(foldableList)(function (v) {
    return function (v1) {
        return new Data_Tuple.Tuple(new Cons(v.value0, v1.value0), new Cons(v.value1, v1.value1));
    };
})(new Data_Tuple.Tuple(Nil.value, Nil.value));
var foldM = function (dictMonad) {
    return function (v) {
        return function (a) {
            return function (v1) {
                if (v1 instanceof Nil) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(a);
                };
                if (v1 instanceof Cons) {
                    return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(v(a)(v1.value0))(function (a$prime) {
                        return foldM(dictMonad)(v)(a$prime)(v1.value1);
                    });
                };
                throw new Error("Failed pattern match at Data.List line 705, column 1 - line 706, column 1: " + [ v.constructor.name, a.constructor.name, v1.constructor.name ]);
            };
        };
    };
};
var findIndex = function (fn) {
    var go = function (__copy_v) {
        return function (__copy_v1) {
            var v = __copy_v;
            var v1 = __copy_v1;
            tco: while (true) {
                if (v1 instanceof Cons) {
                    if (fn(v1.value0)) {
                        return new Data_Maybe.Just(v);
                    };
                    if (Prelude.otherwise) {
                        var __tco_v = v + 1 | 0;
                        var __tco_v1 = v1.value1;
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                };
                if (v1 instanceof Nil) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.List line 307, column 3 - line 309, column 3: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    };
    return go(0);
};
var findLastIndex = function (fn) {
    return function (xs) {
        return Prelude["<$>"](Data_Maybe.functorMaybe)(function (v) {
            return length(xs) - 1 - v;
        })(findIndex(fn)(reverse(xs)));
    };
};
var filterM = function (dictMonad) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Nil) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Nil.value);
            };
            if (v1 instanceof Cons) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(v(v1.value0))(function (v2) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(filterM(dictMonad)(v)(v1.value1))(function (v3) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())((function () {
                            if (v2) {
                                return new Cons(v1.value0, v3);
                            };
                            if (!v2) {
                                return v3;
                            };
                            throw new Error("Failed pattern match at Data.List line 414, column 3 - line 422, column 1: " + [ v2.constructor.name ]);
                        })());
                    });
                });
            };
            throw new Error("Failed pattern match at Data.List line 410, column 1 - line 411, column 1: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var filter = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Nil) {
                    return reverse(acc);
                };
                if (v instanceof Cons) {
                    if (p(v.value0)) {
                        var __tco_acc = new Cons(v.value0, acc);
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                    if (Prelude.otherwise) {
                        var __tco_acc = acc;
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                };
                throw new Error("Failed pattern match at Data.List line 394, column 1 - line 409, column 1: " + [ acc.constructor.name, v.constructor.name ]);
            };
        };
    };
    return go(Nil.value);
};
var intersectBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v1 instanceof Nil) {
                return Nil.value;
            };
            if (v2 instanceof Nil) {
                return Nil.value;
            };
            return filter(function (x) {
                return Data_Foldable.any(foldableList)(Prelude.booleanAlgebraBoolean)(v(x))(v2);
            })(v1);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Prelude["=="](dictEq));
};
var nubBy = function (v) {
    return function (v1) {
        if (v1 instanceof Nil) {
            return Nil.value;
        };
        if (v1 instanceof Cons) {
            return new Cons(v1.value0, nubBy(v)(filter(function (y) {
                return !v(v1.value0)(y);
            })(v1.value1)));
        };
        throw new Error("Failed pattern match at Data.List line 586, column 1 - line 587, column 1: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var nub = function (dictEq) {
    return nubBy(Prelude.eq(dictEq));
};
var eqList = function (dictEq) {
    return new Prelude.Eq(function (xs) {
        return function (ys) {
            var go = function (__copy_v) {
                return function (__copy_v1) {
                    return function (__copy_v2) {
                        var v = __copy_v;
                        var v1 = __copy_v1;
                        var v2 = __copy_v2;
                        tco: while (true) {
                            if (!v2) {
                                return false;
                            };
                            if (v instanceof Nil && v1 instanceof Nil) {
                                return v2;
                            };
                            if (v instanceof Cons && v1 instanceof Cons) {
                                var __tco_v = v.value1;
                                var __tco_v1 = v1.value1;
                                var __tco_v2 = v2 && Prelude["=="](dictEq)(v1.value0)(v.value0);
                                v = __tco_v;
                                v1 = __tco_v1;
                                v2 = __tco_v2;
                                continue tco;
                            };
                            return false;
                        };
                    };
                };
            };
            return go(xs)(ys)(true);
        };
    });
};
var ordList = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqList(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (xs) {
        return function (ys) {
            var go = function (__copy_v) {
                return function (__copy_v1) {
                    var v = __copy_v;
                    var v1 = __copy_v1;
                    tco: while (true) {
                        if (v instanceof Nil && v1 instanceof Nil) {
                            return Prelude.EQ.value;
                        };
                        if (v instanceof Nil) {
                            return Prelude.LT.value;
                        };
                        if (v1 instanceof Nil) {
                            return Prelude.GT.value;
                        };
                        if (v instanceof Cons && v1 instanceof Cons) {
                            var $330 = Prelude.compare(dictOrd)(v.value0)(v1.value0);
                            if ($330 instanceof Prelude.EQ) {
                                var __tco_v = v.value1;
                                var __tco_v1 = v1.value1;
                                v = __tco_v;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            return $330;
                        };
                        throw new Error("Failed pattern match at Data.List line 740, column 3 - line 750, column 1: " + [ v.constructor.name, v1.constructor.name ]);
                    };
                };
            };
            return go(xs)(ys);
        };
    });
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Prelude["=="](dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Prelude["=="](dictEq)(v)(x);
        });
    };
};
var dropWhile = function (p) {
    var go = function (__copy_v) {
        var v = __copy_v;
        tco: while (true) {
            if (v instanceof Cons && p(v.value0)) {
                var __tco_v = v.value1;
                v = __tco_v;
                continue tco;
            };
            return v;
        };
    };
    return go;
};
var drop = function (__copy_v) {
    return function (__copy_v1) {
        var v = __copy_v;
        var v1 = __copy_v1;
        tco: while (true) {
            if (v === 0) {
                return v1;
            };
            if (v1 instanceof Nil) {
                return Nil.value;
            };
            if (v1 instanceof Cons) {
                var __tco_v = v - 1;
                var __tco_v1 = v1.value1;
                v = __tco_v;
                v1 = __tco_v1;
                continue tco;
            };
            throw new Error("Failed pattern match at Data.List line 512, column 1 - line 513, column 1: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var slice = function (start) {
    return function (end) {
        return function (xs) {
            return take(end - start)(drop(start)(xs));
        };
    };
};
var deleteBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nil) {
                return Nil.value;
            };
            if (v2 instanceof Cons && v(v1)(v2.value0)) {
                return v2.value1;
            };
            if (v2 instanceof Cons) {
                return new Cons(v2.value0, deleteBy(v)(v1)(v2.value1));
            };
            throw new Error("Failed pattern match at Data.List line 613, column 1 - line 614, column 1: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Prelude["<>"](semigroupList)(xs)(Data_Foldable.foldl(foldableList)(Prelude.flip(deleteBy(eq)))(nubBy(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Prelude["=="](dictEq));
};
var deleteAt = function (v) {
    return function (v1) {
        if (v === 0 && v1 instanceof Cons) {
            return new Data_Maybe.Just(v1.value1);
        };
        if (v1 instanceof Cons) {
            return Prelude["<$>"](Data_Maybe.functorMaybe)(Cons.create(v1.value0))(deleteAt(v - 1)(v1.value1));
        };
        return Data_Maybe.Nothing.value;
    };
};
var $$delete = function (dictEq) {
    return deleteBy(Prelude["=="](dictEq));
};
var $bslash$bslash = function (dictEq) {
    return Data_Foldable.foldl(foldableList)(Prelude.flip($$delete(dictEq)));
};
var concatMap = function (v) {
    return function (v1) {
        if (v1 instanceof Nil) {
            return Nil.value;
        };
        if (v1 instanceof Cons) {
            return Prelude["<>"](semigroupList)(v(v1.value0))(concatMap(v)(v1.value1));
        };
        throw new Error("Failed pattern match at Data.List line 387, column 1 - line 388, column 1: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var catMaybes = mapMaybe(Prelude.id(Prelude.categoryFn));
var applyList = new Prelude.Apply(function () {
    return functorList;
}, function (v) {
    return function (v1) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            return Prelude["<>"](semigroupList)(Prelude["<$>"](functorList)(v.value0)(v1))(Prelude["<*>"](applyList)(v.value1)(v1));
        };
        throw new Error("Failed pattern match at Data.List line 788, column 3 - line 789, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindList = new Prelude.Bind(function () {
    return applyList;
}, Prelude.flip(concatMap));
var concat = function (v) {
    return Prelude[">>="](bindList)(v)(Prelude.id(Prelude.categoryFn));
};
var applicativeList = new Prelude.Applicative(function () {
    return applyList;
}, function (a) {
    return new Cons(a, Nil.value);
});
var monadList = new Prelude.Monad(function () {
    return applicativeList;
}, function () {
    return bindList;
});
var alterAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0 && v2 instanceof Cons) {
                return Data_Maybe.Just.create((function () {
                    var $366 = v1(v2.value0);
                    if ($366 instanceof Data_Maybe.Nothing) {
                        return v2.value1;
                    };
                    if ($366 instanceof Data_Maybe.Just) {
                        return new Cons($366.value0, v2.value1);
                    };
                    throw new Error("Failed pattern match at Data.List line 356, column 27 - line 360, column 1: " + [ $366.constructor.name ]);
                })());
            };
            if (v2 instanceof Cons) {
                return Prelude["<$>"](Data_Maybe.functorMaybe)(Cons.create(v2.value0))(alterAt(v - 1)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var modifyAt = function (n) {
    return function (f) {
        return alterAt(n)(function ($375) {
            return Data_Maybe.Just.create(f($375));
        });
    };
};
var altList = new Control_Alt.Alt(function () {
    return functorList;
}, Prelude.append(semigroupList));
var plusList = new Control_Plus.Plus(function () {
    return altList;
}, Nil.value);
var alternativeList = new Control_Alternative.Alternative(function () {
    return plusList;
}, function () {
    return applicativeList;
});
var monadPlusList = new Control_MonadPlus.MonadPlus(function () {
    return alternativeList;
}, function () {
    return monadList;
});
module.exports = {
    Nil: Nil, 
    Cons: Cons, 
    fromList: fromList, 
    toList: toList, 
    foldM: foldM, 
    transpose: transpose, 
    unzip: unzip, 
    zip: zip, 
    zipWithA: zipWithA, 
    zipWith: zipWith, 
    intersectBy: intersectBy, 
    intersect: intersect, 
    "\\\\": $bslash$bslash, 
    deleteBy: deleteBy, 
    "delete": $$delete, 
    unionBy: unionBy, 
    union: union, 
    nubBy: nubBy, 
    nub: nub, 
    groupBy: groupBy, 
    "group'": group$prime, 
    group: group, 
    span: span, 
    dropWhile: dropWhile, 
    drop: drop, 
    takeWhile: takeWhile, 
    take: take, 
    slice: slice, 
    sortBy: sortBy, 
    sort: sort, 
    catMaybes: catMaybes, 
    mapMaybe: mapMaybe, 
    filterM: filterM, 
    filter: filter, 
    concatMap: concatMap, 
    concat: concat, 
    reverse: reverse, 
    alterAt: alterAt, 
    modifyAt: modifyAt, 
    updateAt: updateAt, 
    deleteAt: deleteAt, 
    insertAt: insertAt, 
    findLastIndex: findLastIndex, 
    findIndex: findIndex, 
    elemLastIndex: elemLastIndex, 
    elemIndex: elemIndex, 
    index: index, 
    "!!": $bang$bang, 
    uncons: uncons, 
    init: init, 
    tail: tail, 
    last: last, 
    head: head, 
    insertBy: insertBy, 
    insert: insert, 
    snoc: snoc, 
    ":": $colon, 
    length: length, 
    "null": $$null, 
    many: many, 
    some: some, 
    replicateM: replicateM, 
    replicate: replicate, 
    range: range, 
    "..": $dot$dot, 
    singleton: singleton, 
    fromFoldable: fromFoldable, 
    toUnfoldable: toUnfoldable, 
    showList: showList, 
    eqList: eqList, 
    ordList: ordList, 
    semigroupList: semigroupList, 
    monoidList: monoidList, 
    functorList: functorList, 
    foldableList: foldableList, 
    unfoldableList: unfoldableList, 
    traversableList: traversableList, 
    applyList: applyList, 
    applicativeList: applicativeList, 
    bindList: bindList, 
    monadList: monadList, 
    altList: altList, 
    plusList: plusList, 
    alternativeList: alternativeList, 
    monadPlusList: monadPlusList
};

},{"../Control.Alt":26,"../Control.Alternative":27,"../Control.Lazy":37,"../Control.MonadPlus":76,"../Control.Plus":77,"../Data.Foldable":120,"../Data.Maybe":152,"../Data.Monoid":159,"../Data.Traversable":170,"../Data.Tuple":171,"../Data.Unfoldable":172,"../Prelude":203}],147:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Foldable = require("../Data.Foldable");
var Data_List = require("../Data.List");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_Unsafe = require("../Data.Maybe.Unsafe");
var Data_Monoid = require("../Data.Monoid");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Leaf = (function () {
    function Leaf() {

    };
    Leaf.value = new Leaf();
    return Leaf;
})();
var Two = (function () {
    function Two(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    Two.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new Two(value0, value1, value2, value3);
                };
            };
        };
    };
    return Two;
})();
var Three = (function () {
    function Three(value0, value1, value2, value3, value4, value5, value6) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
        this.value6 = value6;
    };
    Three.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return function (value6) {
                                return new Three(value0, value1, value2, value3, value4, value5, value6);
                            };
                        };
                    };
                };
            };
        };
    };
    return Three;
})();
var TwoLeft = (function () {
    function TwoLeft(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    TwoLeft.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new TwoLeft(value0, value1, value2);
            };
        };
    };
    return TwoLeft;
})();
var TwoRight = (function () {
    function TwoRight(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    TwoRight.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new TwoRight(value0, value1, value2);
            };
        };
    };
    return TwoRight;
})();
var ThreeLeft = (function () {
    function ThreeLeft(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeLeft.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeLeft(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeLeft;
})();
var ThreeMiddle = (function () {
    function ThreeMiddle(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeMiddle.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeMiddle(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeMiddle;
})();
var ThreeRight = (function () {
    function ThreeRight(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeRight.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeRight(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeRight;
})();
var KickUp = (function () {
    function KickUp(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    KickUp.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new KickUp(value0, value1, value2, value3);
                };
            };
        };
    };
    return KickUp;
})();
var values = function (v) {
    if (v instanceof Leaf) {
        return Data_List.Nil.value;
    };
    if (v instanceof Two) {
        return Prelude["++"](Data_List.semigroupList)(values(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value2))(values(v.value3)));
    };
    if (v instanceof Three) {
        return Prelude["++"](Data_List.semigroupList)(values(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value2))(Prelude["++"](Data_List.semigroupList)(values(v.value3))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value5))(values(v.value6)))));
    };
    throw new Error("Failed pattern match at Data.Map line 311, column 1 - line 312, column 1: " + [ v.constructor.name ]);
};
var toList = function (v) {
    if (v instanceof Leaf) {
        return Data_List.Nil.value;
    };
    if (v instanceof Two) {
        return Prelude["++"](Data_List.semigroupList)(toList(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(new Data_Tuple.Tuple(v.value1, v.value2)))(toList(v.value3)));
    };
    if (v instanceof Three) {
        return Prelude["++"](Data_List.semigroupList)(toList(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(new Data_Tuple.Tuple(v.value1, v.value2)))(Prelude["++"](Data_List.semigroupList)(toList(v.value3))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(new Data_Tuple.Tuple(v.value4, v.value5)))(toList(v.value6)))));
    };
    throw new Error("Failed pattern match at Data.Map line 290, column 1 - line 291, column 1: " + [ v.constructor.name ]);
};
var size = function ($504) {
    return Data_List.length(values($504));
};
var singleton = function (k) {
    return function (v) {
        return new Two(Leaf.value, k, v, Leaf.value);
    };
};
var showTree = function (dictShow) {
    return function (dictShow1) {
        return function (v) {
            if (v instanceof Leaf) {
                return "Leaf";
            };
            if (v instanceof Two) {
                return "Two (" + (showTree(dictShow)(dictShow1)(v.value0) + (") (" + (Prelude.show(dictShow)(v.value1) + (") (" + (Prelude.show(dictShow1)(v.value2) + (") (" + (showTree(dictShow)(dictShow1)(v.value3) + ")")))))));
            };
            if (v instanceof Three) {
                return "Three (" + (showTree(dictShow)(dictShow1)(v.value0) + (") (" + (Prelude.show(dictShow)(v.value1) + (") (" + (Prelude.show(dictShow1)(v.value2) + (") (" + (showTree(dictShow)(dictShow1)(v.value3) + (") (" + (Prelude.show(dictShow)(v.value4) + (") (" + (Prelude.show(dictShow1)(v.value5) + (") (" + (showTree(dictShow)(dictShow1)(v.value6) + ")")))))))))))));
            };
            throw new Error("Failed pattern match at Data.Map line 77, column 1 - line 78, column 1: " + [ v.constructor.name ]);
        };
    };
};
var showMap = function (dictShow) {
    return function (dictShow1) {
        return new Prelude.Show(function (m) {
            return "fromList " + Prelude.show(Data_List.showList(Data_Tuple.showTuple(dictShow)(dictShow1)))(toList(m));
        });
    };
};
var lookup = function (__copy_dictOrd) {
    return function (__copy_k) {
        return function (__copy_tree) {
            var dictOrd = __copy_dictOrd;
            var k = __copy_k;
            var tree = __copy_tree;
            tco: while (true) {
                var comp = Prelude.compare(dictOrd);
                if (tree instanceof Leaf) {
                    return Data_Maybe.Nothing.value;
                };
                if (tree instanceof Two) {
                    var $91 = comp(k)(tree.value1);
                    if ($91 instanceof Prelude.EQ) {
                        return new Data_Maybe.Just(tree.value2);
                    };
                    if ($91 instanceof Prelude.LT) {
                        var __tco_dictOrd = dictOrd;
                        var __tco_k = k;
                        var __tco_tree = tree.value0;
                        dictOrd = __tco_dictOrd;
                        k = __tco_k;
                        tree = __tco_tree;
                        continue tco;
                    };
                    var __tco_dictOrd = dictOrd;
                    var __tco_k = k;
                    var __tco_tree = tree.value3;
                    dictOrd = __tco_dictOrd;
                    k = __tco_k;
                    tree = __tco_tree;
                    continue tco;
                };
                if (tree instanceof Three) {
                    var $96 = comp(k)(tree.value1);
                    if ($96 instanceof Prelude.EQ) {
                        return new Data_Maybe.Just(tree.value2);
                    };
                    var $97 = comp(k)(tree.value4);
                    if ($97 instanceof Prelude.EQ) {
                        return new Data_Maybe.Just(tree.value5);
                    };
                    if ($96 instanceof Prelude.LT) {
                        var __tco_dictOrd = dictOrd;
                        var __tco_k = k;
                        var __tco_tree = tree.value0;
                        dictOrd = __tco_dictOrd;
                        k = __tco_k;
                        tree = __tco_tree;
                        continue tco;
                    };
                    if ($97 instanceof Prelude.GT) {
                        var __tco_dictOrd = dictOrd;
                        var __tco_k = k;
                        var __tco_tree = tree.value6;
                        dictOrd = __tco_dictOrd;
                        k = __tco_k;
                        tree = __tco_tree;
                        continue tco;
                    };
                    var __tco_dictOrd = dictOrd;
                    var __tco_k = k;
                    var __tco_tree = tree.value3;
                    dictOrd = __tco_dictOrd;
                    k = __tco_k;
                    tree = __tco_tree;
                    continue tco;
                };
                throw new Error("Failed pattern match at Data.Map line 121, column 6 - line 143, column 1: " + [ tree.constructor.name ]);
            };
        };
    };
};
var member = function (dictOrd) {
    return function (k) {
        return function (m) {
            return Data_Maybe.isJust(lookup(dictOrd)(k)(m));
        };
    };
};
var keys = function (v) {
    if (v instanceof Leaf) {
        return Data_List.Nil.value;
    };
    if (v instanceof Two) {
        return Prelude["++"](Data_List.semigroupList)(keys(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value1))(keys(v.value3)));
    };
    if (v instanceof Three) {
        return Prelude["++"](Data_List.semigroupList)(keys(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value1))(Prelude["++"](Data_List.semigroupList)(keys(v.value3))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value4))(keys(v.value6)))));
    };
    throw new Error("Failed pattern match at Data.Map line 305, column 1 - line 306, column 1: " + [ v.constructor.name ]);
};
var isEmpty = function (v) {
    if (v instanceof Leaf) {
        return true;
    };
    return false;
};
var functorMap = new Prelude.Functor(function (v) {
    return function (v1) {
        if (v1 instanceof Leaf) {
            return Leaf.value;
        };
        if (v1 instanceof Two) {
            return new Two(Prelude.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Prelude.map(functorMap)(v)(v1.value3));
        };
        if (v1 instanceof Three) {
            return new Three(Prelude.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Prelude.map(functorMap)(v)(v1.value3), v1.value4, v(v1.value5), Prelude.map(functorMap)(v)(v1.value6));
        };
        throw new Error("Failed pattern match at Data.Map line 62, column 3 - line 63, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var fromZipper = function (__copy_dictOrd) {
    return function (__copy_v) {
        return function (__copy_tree) {
            var dictOrd = __copy_dictOrd;
            var v = __copy_v;
            var tree = __copy_tree;
            tco: while (true) {
                if (v instanceof Data_List.Nil) {
                    return tree;
                };
                if (v instanceof Data_List.Cons) {
                    if (v.value0 instanceof TwoLeft) {
                        var __tco_dictOrd = dictOrd;
                        var __tco_v = v.value1;
                        var __tco_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
                        dictOrd = __tco_dictOrd;
                        v = __tco_v;
                        tree = __tco_tree;
                        continue tco;
                    };
                    if (v.value0 instanceof TwoRight) {
                        var __tco_dictOrd = dictOrd;
                        var __tco_v = v.value1;
                        var __tco_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
                        dictOrd = __tco_dictOrd;
                        v = __tco_v;
                        tree = __tco_tree;
                        continue tco;
                    };
                    if (v.value0 instanceof ThreeLeft) {
                        var __tco_dictOrd = dictOrd;
                        var __tco_v = v.value1;
                        var __tco_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
                        dictOrd = __tco_dictOrd;
                        v = __tco_v;
                        tree = __tco_tree;
                        continue tco;
                    };
                    if (v.value0 instanceof ThreeMiddle) {
                        var __tco_dictOrd = dictOrd;
                        var __tco_v = v.value1;
                        var __tco_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
                        dictOrd = __tco_dictOrd;
                        v = __tco_v;
                        tree = __tco_tree;
                        continue tco;
                    };
                    if (v.value0 instanceof ThreeRight) {
                        var __tco_dictOrd = dictOrd;
                        var __tco_v = v.value1;
                        var __tco_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
                        dictOrd = __tco_dictOrd;
                        v = __tco_v;
                        tree = __tco_tree;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.Map line 156, column 3 - line 163, column 1: " + [ v.value0.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.Map line 154, column 1 - line 155, column 1: " + [ v.constructor.name, tree.constructor.name ]);
            };
        };
    };
};
var insert = function (dictOrd) {
    var up = function (__copy_v) {
        return function (__copy_v1) {
            var v = __copy_v;
            var v1 = __copy_v1;
            tco: while (true) {
                if (v instanceof Data_List.Nil) {
                    return new Two(v1.value0, v1.value1, v1.value2, v1.value3);
                };
                if (v instanceof Data_List.Cons) {
                    if (v.value0 instanceof TwoLeft) {
                        return fromZipper(dictOrd)(v.value1)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, v.value0.value0, v.value0.value1, v.value0.value2));
                    };
                    if (v.value0 instanceof TwoRight) {
                        return fromZipper(dictOrd)(v.value1)(new Three(v.value0.value0, v.value0.value1, v.value0.value2, v1.value0, v1.value1, v1.value2, v1.value3));
                    };
                    if (v.value0 instanceof ThreeLeft) {
                        var __tco_v = v.value1;
                        var __tco_v1 = new KickUp(new Two(v1.value0, v1.value1, v1.value2, v1.value3), v.value0.value0, v.value0.value1, new Two(v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5));
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    if (v.value0 instanceof ThreeMiddle) {
                        var __tco_v = v.value1;
                        var __tco_v1 = new KickUp(new Two(v.value0.value0, v.value0.value1, v.value0.value2, v1.value0), v1.value1, v1.value2, new Two(v1.value3, v.value0.value3, v.value0.value4, v.value0.value5));
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    if (v.value0 instanceof ThreeRight) {
                        var __tco_v = v.value1;
                        var __tco_v1 = new KickUp(new Two(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3), v.value0.value4, v.value0.value5, new Two(v1.value0, v1.value1, v1.value2, v1.value3));
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.Map line 197, column 5 - line 205, column 1: " + [ v.value0.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.Map line 195, column 3 - line 196, column 3: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    };
    var comp = Prelude.compare(dictOrd);
    var down = function (__copy_ctx) {
        return function (__copy_k) {
            return function (__copy_v) {
                return function (__copy_v1) {
                    var ctx = __copy_ctx;
                    var k = __copy_k;
                    var v = __copy_v;
                    var v1 = __copy_v1;
                    tco: while (true) {
                        if (v1 instanceof Leaf) {
                            return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
                        };
                        if (v1 instanceof Two) {
                            var $203 = comp(k)(v1.value1);
                            if ($203 instanceof Prelude.EQ) {
                                return fromZipper(dictOrd)(ctx)(new Two(v1.value0, k, v, v1.value3));
                            };
                            if ($203 instanceof Prelude.LT) {
                                var __tco_ctx = new Data_List.Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                                var __tco_k = k;
                                var __tco_v = v;
                                var __tco_v1 = v1.value0;
                                ctx = __tco_ctx;
                                k = __tco_k;
                                v = __tco_v;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            var __tco_ctx = new Data_List.Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                            var __tco_k = k;
                            var __tco_v = v;
                            var __tco_v1 = v1.value3;
                            ctx = __tco_ctx;
                            k = __tco_k;
                            v = __tco_v;
                            v1 = __tco_v1;
                            continue tco;
                        };
                        if (v1 instanceof Three) {
                            var $208 = comp(k)(v1.value1);
                            if ($208 instanceof Prelude.EQ) {
                                return fromZipper(dictOrd)(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                            };
                            var $209 = comp(k)(v1.value4);
                            if ($209 instanceof Prelude.EQ) {
                                return fromZipper(dictOrd)(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
                            };
                            if ($208 instanceof Prelude.LT) {
                                var __tco_ctx = new Data_List.Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                                var __tco_k = k;
                                var __tco_v = v;
                                var __tco_v1 = v1.value0;
                                ctx = __tco_ctx;
                                k = __tco_k;
                                v = __tco_v;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            if ($208 instanceof Prelude.GT) {
                                if ($209 instanceof Prelude.LT) {
                                    var __tco_ctx = new Data_List.Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                                    var __tco_k = k;
                                    var __tco_v = v;
                                    var __tco_v1 = v1.value3;
                                    ctx = __tco_ctx;
                                    k = __tco_k;
                                    v = __tco_v;
                                    v1 = __tco_v1;
                                    continue tco;
                                };
                                var __tco_ctx = new Data_List.Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                                var __tco_k = k;
                                var __tco_v = v;
                                var __tco_v1 = v1.value6;
                                ctx = __tco_ctx;
                                k = __tco_k;
                                v = __tco_v;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            var __tco_ctx = new Data_List.Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                            var __tco_k = k;
                            var __tco_v = v;
                            var __tco_v1 = v1.value6;
                            ctx = __tco_ctx;
                            k = __tco_k;
                            v = __tco_v;
                            v1 = __tco_v1;
                            continue tco;
                        };
                        throw new Error("Failed pattern match at Data.Map line 173, column 3 - line 174, column 3: " + [ ctx.constructor.name, k.constructor.name, v.constructor.name, v1.constructor.name ]);
                    };
                };
            };
        };
    };
    return down(Data_List.Nil.value);
};
var foldableMap = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (m) {
            return Data_Foldable.foldMap(Data_List.foldableList)(dictMonoid)(f)(values(m));
        };
    };
}, function (f) {
    return function (z) {
        return function (m) {
            return Data_Foldable.foldl(Data_List.foldableList)(f)(z)(values(m));
        };
    };
}, function (f) {
    return function (z) {
        return function (m) {
            return Data_Foldable.foldr(Data_List.foldableList)(f)(z)(values(m));
        };
    };
});
var eqMap = function (dictEq) {
    return function (dictEq1) {
        return new Prelude.Eq(function (m1) {
            return function (m2) {
                return Prelude["=="](Data_List.eqList(Data_Tuple.eqTuple(dictEq)(dictEq1)))(toList(m1))(toList(m2));
            };
        });
    };
};
var ordMap = function (dictOrd) {
    return function (dictOrd1) {
        return new Prelude.Ord(function () {
            return eqMap(dictOrd["__superclass_Prelude.Eq_0"]())(dictOrd1["__superclass_Prelude.Eq_0"]());
        }, function (m1) {
            return function (m2) {
                return Prelude.compare(Data_List.ordList(Data_Tuple.ordTuple(dictOrd)(dictOrd1)))(toList(m1))(toList(m2));
            };
        });
    };
};
var empty = Leaf.value;
var fromFoldable = function (dictOrd) {
    return function (dictFoldable) {
        return Data_Foldable.foldl(dictFoldable)(function (m) {
            return function (v) {
                return insert(dictOrd)(v.value0)(v.value1)(m);
            };
        })(empty);
    };
};
var fromList = function (dictOrd) {
    return fromFoldable(dictOrd)(Data_List.foldableList);
};
var $$delete = function (dictOrd) {
    var up = function (__copy_v) {
        return function (__copy_v1) {
            var v = __copy_v;
            var v1 = __copy_v1;
            tco: while (true) {
                if (v instanceof Data_List.Nil) {
                    return v1;
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof TwoLeft && (v.value0.value2 instanceof Leaf && v1 instanceof Leaf))) {
                    return fromZipper(dictOrd)(v.value1)(new Two(Leaf.value, v.value0.value0, v.value0.value1, Leaf.value));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof TwoRight && (v.value0.value0 instanceof Leaf && v1 instanceof Leaf))) {
                    return fromZipper(dictOrd)(v.value1)(new Two(Leaf.value, v.value0.value1, v.value0.value2, Leaf.value));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof TwoLeft && v.value0.value2 instanceof Two)) {
                    var __tco_v = v.value1;
                    var __tco_v1 = new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2.value0, v.value0.value2.value1, v.value0.value2.value2, v.value0.value2.value3);
                    v = __tco_v;
                    v1 = __tco_v1;
                    continue tco;
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof TwoRight && v.value0.value0 instanceof Two)) {
                    var __tco_v = v.value1;
                    var __tco_v1 = new Three(v.value0.value0.value0, v.value0.value0.value1, v.value0.value0.value2, v.value0.value0.value3, v.value0.value1, v.value0.value2, v1);
                    v = __tco_v;
                    v1 = __tco_v1;
                    continue tco;
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof TwoLeft && v.value0.value2 instanceof Three)) {
                    return fromZipper(dictOrd)(v.value1)(new Two(new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2.value0), v.value0.value2.value1, v.value0.value2.value2, new Two(v.value0.value2.value3, v.value0.value2.value4, v.value0.value2.value5, v.value0.value2.value6)));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof TwoRight && v.value0.value0 instanceof Three)) {
                    return fromZipper(dictOrd)(v.value1)(new Two(new Two(v.value0.value0.value0, v.value0.value0.value1, v.value0.value0.value2, v.value0.value0.value3), v.value0.value0.value4, v.value0.value0.value5, new Two(v.value0.value0.value6, v.value0.value1, v.value0.value2, v1)));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeLeft && (v.value0.value2 instanceof Leaf && (v.value0.value5 instanceof Leaf && v1 instanceof Leaf)))) {
                    return fromZipper(dictOrd)(v.value1)(new Three(Leaf.value, v.value0.value0, v.value0.value1, Leaf.value, v.value0.value3, v.value0.value4, Leaf.value));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeMiddle && (v.value0.value0 instanceof Leaf && (v.value0.value5 instanceof Leaf && v1 instanceof Leaf)))) {
                    return fromZipper(dictOrd)(v.value1)(new Three(Leaf.value, v.value0.value1, v.value0.value2, Leaf.value, v.value0.value3, v.value0.value4, Leaf.value));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeRight && (v.value0.value0 instanceof Leaf && (v.value0.value3 instanceof Leaf && v1 instanceof Leaf)))) {
                    return fromZipper(dictOrd)(v.value1)(new Three(Leaf.value, v.value0.value1, v.value0.value2, Leaf.value, v.value0.value4, v.value0.value5, Leaf.value));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeLeft && v.value0.value2 instanceof Two)) {
                    return fromZipper(dictOrd)(v.value1)(new Two(new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2.value0, v.value0.value2.value1, v.value0.value2.value2, v.value0.value2.value3), v.value0.value3, v.value0.value4, v.value0.value5));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeMiddle && v.value0.value0 instanceof Two)) {
                    return fromZipper(dictOrd)(v.value1)(new Two(new Three(v.value0.value0.value0, v.value0.value0.value1, v.value0.value0.value2, v.value0.value0.value3, v.value0.value1, v.value0.value2, v1), v.value0.value3, v.value0.value4, v.value0.value5));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeMiddle && v.value0.value5 instanceof Two)) {
                    return fromZipper(dictOrd)(v.value1)(new Two(v.value0.value0, v.value0.value1, v.value0.value2, new Three(v1, v.value0.value3, v.value0.value4, v.value0.value5.value0, v.value0.value5.value1, v.value0.value5.value2, v.value0.value5.value3)));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeRight && v.value0.value3 instanceof Two)) {
                    return fromZipper(dictOrd)(v.value1)(new Two(v.value0.value0, v.value0.value1, v.value0.value2, new Three(v.value0.value3.value0, v.value0.value3.value1, v.value0.value3.value2, v.value0.value3.value3, v.value0.value4, v.value0.value5, v1)));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeLeft && v.value0.value2 instanceof Three)) {
                    return fromZipper(dictOrd)(v.value1)(new Three(new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2.value0), v.value0.value2.value1, v.value0.value2.value2, new Two(v.value0.value2.value3, v.value0.value2.value4, v.value0.value2.value5, v.value0.value2.value6), v.value0.value3, v.value0.value4, v.value0.value5));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeMiddle && v.value0.value0 instanceof Three)) {
                    return fromZipper(dictOrd)(v.value1)(new Three(new Two(v.value0.value0.value0, v.value0.value0.value1, v.value0.value0.value2, v.value0.value0.value3), v.value0.value0.value4, v.value0.value0.value5, new Two(v.value0.value0.value6, v.value0.value1, v.value0.value2, v1), v.value0.value3, v.value0.value4, v.value0.value5));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeMiddle && v.value0.value5 instanceof Three)) {
                    return fromZipper(dictOrd)(v.value1)(new Three(v.value0.value0, v.value0.value1, v.value0.value2, new Two(v1, v.value0.value3, v.value0.value4, v.value0.value5.value0), v.value0.value5.value1, v.value0.value5.value2, new Two(v.value0.value5.value3, v.value0.value5.value4, v.value0.value5.value5, v.value0.value5.value6)));
                };
                if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeRight && v.value0.value3 instanceof Three)) {
                    return fromZipper(dictOrd)(v.value1)(new Three(v.value0.value0, v.value0.value1, v.value0.value2, new Two(v.value0.value3.value0, v.value0.value3.value1, v.value0.value3.value2, v.value0.value3.value3), v.value0.value3.value4, v.value0.value3.value5, new Two(v.value0.value3.value6, v.value0.value4, v.value0.value5, v1)));
                };
                return Data_Maybe_Unsafe.unsafeThrow("Impossible case in 'up'");
            };
        };
    };
    var removeMaxNode = function (__copy_v) {
        return function (__copy_v1) {
            var v = __copy_v;
            var v1 = __copy_v1;
            tco: while (true) {
                if (v1 instanceof Two && (v1.value0 instanceof Leaf && v1.value3 instanceof Leaf)) {
                    return up(v)(Leaf.value);
                };
                if (v1 instanceof Two) {
                    var __tco_v = new Data_List.Cons(new TwoRight(v1.value0, v1.value1, v1.value2), v);
                    var __tco_v1 = v1.value3;
                    v = __tco_v;
                    v1 = __tco_v1;
                    continue tco;
                };
                if (v1 instanceof Three && (v1.value0 instanceof Leaf && (v1.value3 instanceof Leaf && v1.value6 instanceof Leaf))) {
                    return up(new Data_List.Cons(new TwoRight(Leaf.value, v1.value1, v1.value2), v))(Leaf.value);
                };
                if (v1 instanceof Three) {
                    var __tco_v = new Data_List.Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), v);
                    var __tco_v1 = v1.value6;
                    v = __tco_v;
                    v1 = __tco_v1;
                    continue tco;
                };
                if (v1 instanceof Leaf) {
                    return Data_Maybe_Unsafe.unsafeThrow("Impossible case in 'removeMaxNode'");
                };
                throw new Error("Failed pattern match at Data.Map line 259, column 3 - line 260, column 3: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    };
    var maxNode = function (__copy_v) {
        var v = __copy_v;
        tco: while (true) {
            if (v instanceof Two && v.value3 instanceof Leaf) {
                return {
                    key: v.value1, 
                    value: v.value2
                };
            };
            if (v instanceof Two) {
                var __tco_v = v.value3;
                v = __tco_v;
                continue tco;
            };
            if (v instanceof Three && v.value6 instanceof Leaf) {
                return {
                    key: v.value4, 
                    value: v.value5
                };
            };
            if (v instanceof Three) {
                var __tco_v = v.value6;
                v = __tco_v;
                continue tco;
            };
            if (v instanceof Leaf) {
                return Data_Maybe_Unsafe.unsafeThrow("Impossible case in 'maxNode'");
            };
            throw new Error("Failed pattern match at Data.Map line 251, column 3 - line 252, column 3: " + [ v.constructor.name ]);
        };
    };
    var down = function (__copy_ctx) {
        return function (__copy_v) {
            return function (__copy_v1) {
                var ctx = __copy_ctx;
                var v = __copy_v;
                var v1 = __copy_v1;
                tco: while (true) {
                    if (v1 instanceof Leaf) {
                        return fromZipper(dictOrd)(ctx)(Leaf.value);
                    };
                    if (v1 instanceof Two && (v1.value0 instanceof Leaf && (v1.value3 instanceof Leaf && Prelude["=="](dictOrd["__superclass_Prelude.Eq_0"]())(v)(v1.value1)))) {
                        return up(ctx)(Leaf.value);
                    };
                    if (v1 instanceof Two) {
                        if (Prelude["=="](dictOrd["__superclass_Prelude.Eq_0"]())(v)(v1.value1)) {
                            var max = maxNode(v1.value0);
                            return removeMaxNode(new Data_List.Cons(new TwoLeft(max.key, max.value, v1.value3), ctx))(v1.value0);
                        };
                        if (Prelude["<"](dictOrd)(v)(v1.value1)) {
                            var __tco_ctx = new Data_List.Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                            var __tco_v = v;
                            var __tco_v1 = v1.value0;
                            ctx = __tco_ctx;
                            v = __tco_v;
                            v1 = __tco_v1;
                            continue tco;
                        };
                        if (Prelude.otherwise) {
                            var __tco_ctx = new Data_List.Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                            var __tco_v = v;
                            var __tco_v1 = v1.value3;
                            ctx = __tco_ctx;
                            v = __tco_v;
                            v1 = __tco_v1;
                            continue tco;
                        };
                    };
                    if (v1 instanceof Three && (v1.value0 instanceof Leaf && (v1.value3 instanceof Leaf && v1.value6 instanceof Leaf))) {
                        if (Prelude["=="](dictOrd["__superclass_Prelude.Eq_0"]())(v)(v1.value1)) {
                            return fromZipper(dictOrd)(ctx)(new Two(Leaf.value, v1.value4, v1.value5, Leaf.value));
                        };
                        if (Prelude["=="](dictOrd["__superclass_Prelude.Eq_0"]())(v)(v1.value4)) {
                            return fromZipper(dictOrd)(ctx)(new Two(Leaf.value, v1.value1, v1.value2, Leaf.value));
                        };
                    };
                    if (v1 instanceof Three) {
                        if (Prelude["=="](dictOrd["__superclass_Prelude.Eq_0"]())(v)(v1.value1)) {
                            var max = maxNode(v1.value0);
                            return removeMaxNode(new Data_List.Cons(new ThreeLeft(max.key, max.value, v1.value3, v1.value4, v1.value5, v1.value6), ctx))(v1.value0);
                        };
                        if (Prelude["=="](dictOrd["__superclass_Prelude.Eq_0"]())(v)(v1.value4)) {
                            var max = maxNode(v1.value3);
                            return removeMaxNode(new Data_List.Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, max.key, max.value, v1.value6), ctx))(v1.value3);
                        };
                        if (Prelude["<"](dictOrd)(v)(v1.value1)) {
                            var __tco_ctx = new Data_List.Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                            var __tco_v = v;
                            var __tco_v1 = v1.value0;
                            ctx = __tco_ctx;
                            v = __tco_v;
                            v1 = __tco_v1;
                            continue tco;
                        };
                        if (Prelude["<"](dictOrd)(v1.value1)(v) && Prelude["<"](dictOrd)(v)(v1.value4)) {
                            var __tco_ctx = new Data_List.Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                            var __tco_v = v;
                            var __tco_v1 = v1.value3;
                            ctx = __tco_ctx;
                            v = __tco_v;
                            v1 = __tco_v1;
                            continue tco;
                        };
                        if (Prelude.otherwise) {
                            var __tco_ctx = new Data_List.Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                            var __tco_v = v;
                            var __tco_v1 = v1.value6;
                            ctx = __tco_ctx;
                            v = __tco_v;
                            v1 = __tco_v1;
                            continue tco;
                        };
                    };
                    throw new Error("Failed pattern match at Data.Map line 209, column 3 - line 210, column 3: " + [ ctx.constructor.name, v.constructor.name, v1.constructor.name ]);
                };
            };
        };
    };
    return down(Data_List.Nil.value);
};
var checkValid = function (tree) {
    var allHeights = function (v) {
        if (v instanceof Leaf) {
            return Prelude.pure(Data_List.applicativeList)(0);
        };
        if (v instanceof Two) {
            return Prelude.map(Data_List.functorList)(function (n) {
                return n + 1 | 0;
            })(Prelude["++"](Data_List.semigroupList)(allHeights(v.value0))(allHeights(v.value3)));
        };
        if (v instanceof Three) {
            return Prelude.map(Data_List.functorList)(function (n) {
                return n + 1 | 0;
            })(Prelude["++"](Data_List.semigroupList)(allHeights(v.value0))(Prelude["++"](Data_List.semigroupList)(allHeights(v.value3))(allHeights(v.value6))));
        };
        throw new Error("Failed pattern match at Data.Map line 112, column 3 - line 113, column 3: " + [ v.constructor.name ]);
    };
    return Data_List.length(Data_List.nub(Prelude.eqInt)(allHeights(tree))) === 1;
};
var alter = function (dictOrd) {
    return function (f) {
        return function (k) {
            return function (m) {
                var $492 = f(lookup(dictOrd)(k)(m));
                if ($492 instanceof Data_Maybe.Nothing) {
                    return $$delete(dictOrd)(k)(m);
                };
                if ($492 instanceof Data_Maybe.Just) {
                    return insert(dictOrd)(k)($492.value0)(m);
                };
                throw new Error("Failed pattern match at Data.Map line 268, column 15 - line 273, column 1: " + [ $492.constructor.name ]);
            };
        };
    };
};
var fromFoldableWith = function (dictOrd) {
    return function (dictFoldable) {
        return function (f) {
            var combine = function (v) {
                return function (v1) {
                    if (v1 instanceof Data_Maybe.Just) {
                        return Data_Maybe.Just.create(f(v)(v1.value0));
                    };
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return new Data_Maybe.Just(v);
                    };
                    throw new Error("Failed pattern match at Data.Map line 285, column 3 - line 286, column 3: " + [ v.constructor.name, v1.constructor.name ]);
                };
            };
            return Data_Foldable.foldl(dictFoldable)(function (m) {
                return function (v) {
                    return alter(dictOrd)(combine(v.value1))(v.value0)(m);
                };
            })(empty);
        };
    };
};
var fromListWith = function (dictOrd) {
    return fromFoldableWith(dictOrd)(Data_List.foldableList);
};
var unionWith = function (dictOrd) {
    return function (f) {
        return function (m1) {
            return function (m2) {
                var go = function (m) {
                    return function (v) {
                        return alter(dictOrd)(function ($505) {
                            return Data_Maybe.Just.create(Data_Maybe.maybe(v.value1)(f(v.value1))($505));
                        })(v.value0)(m);
                    };
                };
                return Data_Foldable.foldl(Data_List.foldableList)(go)(m2)(toList(m1));
            };
        };
    };
};
var union = function (dictOrd) {
    return unionWith(dictOrd)(Prelude["const"]);
};
var semigroupMap = function (dictOrd) {
    return new Prelude.Semigroup(union(dictOrd));
};
var monoidMap = function (dictOrd) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMap(dictOrd);
    }, empty);
};
var traversableMap = function (dictOrd) {
    return new Data_Traversable.Traversable(function () {
        return foldableMap;
    }, function () {
        return functorMap;
    }, function (dictApplicative) {
        return Data_Traversable.traverse(traversableMap(dictOrd))(dictApplicative)(Prelude.id(Prelude.categoryFn));
    }, function (dictApplicative) {
        return function (f) {
            return function (ms) {
                return Data_Foldable.foldr(Data_List.foldableList)(function (x) {
                    return function (acc) {
                        return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(union(dictOrd))(x))(acc);
                    };
                })(Prelude.pure(dictApplicative)(empty))(Prelude["<$>"](Data_List.functorList)(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Tuple.uncurry(singleton)))(Prelude["<$>"](Data_List.functorList)(Data_Traversable.traverse(Data_Tuple.traversableTuple)(dictApplicative)(f))(toList(ms))));
            };
        };
    });
};
var unions = function (dictOrd) {
    return function (dictFoldable) {
        return Data_Foldable.foldl(dictFoldable)(union(dictOrd))(empty);
    };
};
var update = function (dictOrd) {
    return function (f) {
        return function (k) {
            return function (m) {
                return alter(dictOrd)(Data_Maybe.maybe(Data_Maybe.Nothing.value)(f))(k)(m);
            };
        };
    };
};
module.exports = {
    size: size, 
    unions: unions, 
    unionWith: unionWith, 
    union: union, 
    values: values, 
    keys: keys, 
    update: update, 
    alter: alter, 
    member: member, 
    "delete": $$delete, 
    fromListWith: fromListWith, 
    fromList: fromList, 
    toList: toList, 
    fromFoldableWith: fromFoldableWith, 
    fromFoldable: fromFoldable, 
    lookup: lookup, 
    insert: insert, 
    checkValid: checkValid, 
    singleton: singleton, 
    isEmpty: isEmpty, 
    empty: empty, 
    showTree: showTree, 
    eqMap: eqMap, 
    showMap: showMap, 
    ordMap: ordMap, 
    semigroupMap: semigroupMap, 
    monoidMap: monoidMap, 
    functorMap: functorMap, 
    foldableMap: foldableMap, 
    traversableMap: traversableMap
};

},{"../Data.Foldable":120,"../Data.List":146,"../Data.Maybe":152,"../Data.Maybe.Unsafe":151,"../Data.Monoid":159,"../Data.Traversable":170,"../Data.Tuple":171,"../Prelude":203}],148:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var First = function (x) {
    return x;
};
var showFirst = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "First (" + (Prelude.show(Data_Maybe.showMaybe(dictShow))(v) + ")");
    });
};
var semigroupFirst = new Prelude.Semigroup(function (v) {
    return function (v1) {
        if (v instanceof Data_Maybe.Just) {
            return v;
        };
        return v1;
    };
});
var runFirst = function (v) {
    return v;
};
var monoidFirst = new Data_Monoid.Monoid(function () {
    return semigroupFirst;
}, Data_Maybe.Nothing.value);
var functorFirst = new Prelude.Functor(function (f) {
    return function (v) {
        return Prelude["<$>"](Data_Maybe.functorMaybe)(f)(v);
    };
});
var invariantFirst = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorFirst));
var extendFirst = new Control_Extend.Extend(function () {
    return functorFirst;
}, function (f) {
    return function (v) {
        return Control_Extend.extend(Data_Maybe.extendMaybe)(function ($34) {
            return f(First($34));
        })(v);
    };
});
var eqFirst = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](Data_Maybe.eqMaybe(dictEq))(v)(v1);
        };
    });
};
var ordFirst = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqFirst(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(Data_Maybe.ordMaybe(dictOrd))(v)(v1);
        };
    });
};
var boundedFirst = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(Data_Maybe.boundedMaybe(dictBounded)), Prelude.top(Data_Maybe.boundedMaybe(dictBounded)));
};
var applyFirst = new Prelude.Apply(function () {
    return functorFirst;
}, function (v) {
    return function (v1) {
        return Prelude["<*>"](Data_Maybe.applyMaybe)(v)(v1);
    };
});
var bindFirst = new Prelude.Bind(function () {
    return applyFirst;
}, function (v) {
    return function (f) {
        return Prelude.bind(Data_Maybe.bindMaybe)(v)(function ($35) {
            return runFirst(f($35));
        });
    };
});
var applicativeFirst = new Prelude.Applicative(function () {
    return applyFirst;
}, function ($36) {
    return First(Prelude.pure(Data_Maybe.applicativeMaybe)($36));
});
var monadFirst = new Prelude.Monad(function () {
    return applicativeFirst;
}, function () {
    return bindFirst;
});
module.exports = {
    First: First, 
    runFirst: runFirst, 
    eqFirst: eqFirst, 
    ordFirst: ordFirst, 
    boundedFirst: boundedFirst, 
    functorFirst: functorFirst, 
    applyFirst: applyFirst, 
    applicativeFirst: applicativeFirst, 
    bindFirst: bindFirst, 
    monadFirst: monadFirst, 
    extendFirst: extendFirst, 
    invariantFirst: invariantFirst, 
    showFirst: showFirst, 
    semigroupFirst: semigroupFirst, 
    monoidFirst: monoidFirst
};

},{"../Control.Comonad":32,"../Control.Extend":36,"../Data.Functor.Invariant":133,"../Data.Maybe":152,"../Data.Monoid":159,"../Prelude":203}],149:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Last = function (x) {
    return x;
};
var showLast = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Last (" + (Prelude.show(Data_Maybe.showMaybe(dictShow))(v) + ")");
    });
};
var semigroupLast = new Prelude.Semigroup(function (v) {
    return function (v1) {
        if (v1 instanceof Data_Maybe.Just) {
            return v1;
        };
        if (v1 instanceof Data_Maybe.Nothing) {
            return v;
        };
        throw new Error("Failed pattern match at Data.Maybe.Last line 58, column 3 - line 59, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var runLast = function (v) {
    return v;
};
var monoidLast = new Data_Monoid.Monoid(function () {
    return semigroupLast;
}, Data_Maybe.Nothing.value);
var functorLast = new Prelude.Functor(function (f) {
    return function (v) {
        return Prelude["<$>"](Data_Maybe.functorMaybe)(f)(v);
    };
});
var invariantLast = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorLast));
var extendLast = new Control_Extend.Extend(function () {
    return functorLast;
}, function (f) {
    return function (v) {
        return Control_Extend.extend(Data_Maybe.extendMaybe)(function ($34) {
            return f(Last($34));
        })(v);
    };
});
var eqLast = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](Data_Maybe.eqMaybe(dictEq))(v)(v1);
        };
    });
};
var ordLast = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqLast(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(Data_Maybe.ordMaybe(dictOrd))(v)(v1);
        };
    });
};
var boundedLast = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(Data_Maybe.boundedMaybe(dictBounded)), Prelude.top(Data_Maybe.boundedMaybe(dictBounded)));
};
var applyLast = new Prelude.Apply(function () {
    return functorLast;
}, function (v) {
    return function (v1) {
        return Prelude["<*>"](Data_Maybe.applyMaybe)(v)(v1);
    };
});
var bindLast = new Prelude.Bind(function () {
    return applyLast;
}, function (v) {
    return function (f) {
        return Prelude.bind(Data_Maybe.bindMaybe)(v)(function ($35) {
            return runLast(f($35));
        });
    };
});
var applicativeLast = new Prelude.Applicative(function () {
    return applyLast;
}, function ($36) {
    return Last(Prelude.pure(Data_Maybe.applicativeMaybe)($36));
});
var monadLast = new Prelude.Monad(function () {
    return applicativeLast;
}, function () {
    return bindLast;
});
module.exports = {
    Last: Last, 
    runLast: runLast, 
    eqLast: eqLast, 
    ordLast: ordLast, 
    boundedLast: boundedLast, 
    functorLast: functorLast, 
    applyLast: applyLast, 
    applicativeLast: applicativeLast, 
    bindLast: bindLast, 
    monadLast: monadLast, 
    extendLast: extendLast, 
    invariantLast: invariantLast, 
    showLast: showLast, 
    semigroupLast: semigroupLast, 
    monoidLast: monoidLast
};

},{"../Control.Comonad":32,"../Control.Extend":36,"../Data.Functor.Invariant":133,"../Data.Maybe":152,"../Data.Monoid":159,"../Prelude":203}],150:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Maybe.Unsafe

exports.unsafeThrow = function (msg) {
  throw new Error(msg);
};

},{}],151:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var fromJust = function (v) {
    if (v instanceof Data_Maybe.Just) {
        return v.value0;
    };
    if (v instanceof Data_Maybe.Nothing) {
        return $foreign.unsafeThrow("Data.Maybe.Unsafe.fromJust called on Nothing");
    };
    throw new Error("Failed pattern match at Data.Maybe.Unsafe line 11, column 1 - line 12, column 1: " + [ v.constructor.name ]);
};
module.exports = {
    fromJust: fromJust, 
    unsafeThrow: $foreign.unsafeThrow
};

},{"../Data.Maybe":152,"../Prelude":203,"./foreign":150}],152:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Extend = require("../Control.Extend");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_Plus = require("../Control.Plus");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Nothing = (function () {
    function Nothing() {

    };
    Nothing.value = new Nothing();
    return Nothing;
})();
var Just = (function () {
    function Just(value0) {
        this.value0 = value0;
    };
    Just.create = function (value0) {
        return new Just(value0);
    };
    return Just;
})();
var showMaybe = function (dictShow) {
    return new Prelude.Show(function (v) {
        if (v instanceof Just) {
            return "Just (" + (Prelude.show(dictShow)(v.value0) + ")");
        };
        if (v instanceof Nothing) {
            return "Nothing";
        };
        throw new Error("Failed pattern match at Data.Maybe line 290, column 3 - line 291, column 3: " + [ v.constructor.name ]);
    });
};
var semigroupMaybe = function (dictSemigroup) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            if (v instanceof Nothing) {
                return v1;
            };
            if (v1 instanceof Nothing) {
                return v;
            };
            if (v instanceof Just && v1 instanceof Just) {
                return new Just(Prelude["<>"](dictSemigroup)(v.value0)(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Maybe line 232, column 3 - line 233, column 3: " + [ v.constructor.name, v1.constructor.name ]);
        };
    });
};
var monoidMaybe = function (dictSemigroup) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMaybe(dictSemigroup);
    }, Nothing.value);
};
var maybe$prime = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nothing) {
                return v(Prelude.unit);
            };
            if (v2 instanceof Just) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe line 40, column 1 - line 41, column 1: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var maybe = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nothing) {
                return v;
            };
            if (v2 instanceof Just) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe line 27, column 1 - line 28, column 1: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var isNothing = maybe(true)(Prelude["const"](false));
var isJust = maybe(false)(Prelude["const"](true));
var functorMaybe = new Prelude.Functor(function (v) {
    return function (v1) {
        if (v1 instanceof Just) {
            return new Just(v(v1.value0));
        };
        return Nothing.value;
    };
});
var invariantMaybe = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorMaybe));
var fromMaybe$prime = function (a) {
    return maybe$prime(a)(Prelude.id(Prelude.categoryFn));
};
var fromMaybe = function (a) {
    return maybe(a)(Prelude.id(Prelude.categoryFn));
};
var extendMaybe = new Control_Extend.Extend(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v1 instanceof Nothing) {
            return Nothing.value;
        };
        return new Just(v(v1));
    };
});
var eqMaybe = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            if (v instanceof Nothing && v1 instanceof Nothing) {
                return true;
            };
            if (v instanceof Just && v1 instanceof Just) {
                return Prelude["=="](dictEq)(v.value0)(v1.value0);
            };
            return false;
        };
    });
};
var ordMaybe = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqMaybe(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            if (v instanceof Just && v1 instanceof Just) {
                return Prelude.compare(dictOrd)(v.value0)(v1.value0);
            };
            if (v instanceof Nothing && v1 instanceof Nothing) {
                return Prelude.EQ.value;
            };
            if (v instanceof Nothing) {
                return Prelude.LT.value;
            };
            if (v1 instanceof Nothing) {
                return Prelude.GT.value;
            };
            throw new Error("Failed pattern match at Data.Maybe line 270, column 3 - line 271, column 3: " + [ v.constructor.name, v1.constructor.name ]);
        };
    });
};
var boundedMaybe = function (dictBounded) {
    return new Prelude.Bounded(Nothing.value, new Just(Prelude.top(dictBounded)));
};
var boundedOrdMaybe = function (dictBoundedOrd) {
    return new Prelude.BoundedOrd(function () {
        return boundedMaybe(dictBoundedOrd["__superclass_Prelude.Bounded_0"]());
    }, function () {
        return ordMaybe(dictBoundedOrd["__superclass_Prelude.Ord_1"]());
    });
};
var applyMaybe = new Prelude.Apply(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Just) {
            return Prelude["<$>"](functorMaybe)(v.value0)(v1);
        };
        if (v instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Maybe line 122, column 3 - line 123, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindMaybe = new Prelude.Bind(function () {
    return applyMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Just) {
            return v1(v.value0);
        };
        if (v instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Maybe line 181, column 3 - line 182, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var booleanAlgebraMaybe = function (dictBooleanAlgebra) {
    return new Prelude.BooleanAlgebra(function () {
        return boundedMaybe(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]());
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.conj(dictBooleanAlgebra))(x))(y);
        };
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.disj(dictBooleanAlgebra))(x))(y);
        };
    }, Prelude.map(functorMaybe)(Prelude.not(dictBooleanAlgebra)));
};
var semiringMaybe = function (dictSemiring) {
    return new Prelude.Semiring(function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.add(dictSemiring))(x))(y);
        };
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.mul(dictSemiring))(x))(y);
        };
    }, new Just(Prelude.one(dictSemiring)), new Just(Prelude.zero(dictSemiring)));
};
var moduloSemiringMaybe = function (dictModuloSemiring) {
    return new Prelude.ModuloSemiring(function () {
        return semiringMaybe(dictModuloSemiring["__superclass_Prelude.Semiring_0"]());
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.div(dictModuloSemiring))(x))(y);
        };
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.mod(dictModuloSemiring))(x))(y);
        };
    });
};
var ringMaybe = function (dictRing) {
    return new Prelude.Ring(function () {
        return semiringMaybe(dictRing["__superclass_Prelude.Semiring_0"]());
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.sub(dictRing))(x))(y);
        };
    });
};
var divisionRingMaybe = function (dictDivisionRing) {
    return new Prelude.DivisionRing(function () {
        return moduloSemiringMaybe(dictDivisionRing["__superclass_Prelude.ModuloSemiring_1"]());
    }, function () {
        return ringMaybe(dictDivisionRing["__superclass_Prelude.Ring_0"]());
    });
};
var numMaybe = function (dictNum) {
    return new Prelude.Num(function () {
        return divisionRingMaybe(dictNum["__superclass_Prelude.DivisionRing_0"]());
    });
};
var applicativeMaybe = new Prelude.Applicative(function () {
    return applyMaybe;
}, Just.create);
var monadMaybe = new Prelude.Monad(function () {
    return applicativeMaybe;
}, function () {
    return bindMaybe;
});
var altMaybe = new Control_Alt.Alt(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Nothing) {
            return v1;
        };
        return v;
    };
});
var plusMaybe = new Control_Plus.Plus(function () {
    return altMaybe;
}, Nothing.value);
var alternativeMaybe = new Control_Alternative.Alternative(function () {
    return plusMaybe;
}, function () {
    return applicativeMaybe;
});
var monadPlusMaybe = new Control_MonadPlus.MonadPlus(function () {
    return alternativeMaybe;
}, function () {
    return monadMaybe;
});
module.exports = {
    Nothing: Nothing, 
    Just: Just, 
    isNothing: isNothing, 
    isJust: isJust, 
    "fromMaybe'": fromMaybe$prime, 
    fromMaybe: fromMaybe, 
    "maybe'": maybe$prime, 
    maybe: maybe, 
    functorMaybe: functorMaybe, 
    applyMaybe: applyMaybe, 
    applicativeMaybe: applicativeMaybe, 
    altMaybe: altMaybe, 
    plusMaybe: plusMaybe, 
    alternativeMaybe: alternativeMaybe, 
    bindMaybe: bindMaybe, 
    monadMaybe: monadMaybe, 
    monadPlusMaybe: monadPlusMaybe, 
    extendMaybe: extendMaybe, 
    invariantMaybe: invariantMaybe, 
    semigroupMaybe: semigroupMaybe, 
    monoidMaybe: monoidMaybe, 
    semiringMaybe: semiringMaybe, 
    moduloSemiringMaybe: moduloSemiringMaybe, 
    ringMaybe: ringMaybe, 
    divisionRingMaybe: divisionRingMaybe, 
    numMaybe: numMaybe, 
    eqMaybe: eqMaybe, 
    ordMaybe: ordMaybe, 
    boundedMaybe: boundedMaybe, 
    boundedOrdMaybe: boundedOrdMaybe, 
    booleanAlgebraMaybe: booleanAlgebraMaybe, 
    showMaybe: showMaybe
};

},{"../Control.Alt":26,"../Control.Alternative":27,"../Control.Extend":36,"../Control.MonadPlus":76,"../Control.Plus":77,"../Data.Functor.Invariant":133,"../Data.Monoid":159,"../Prelude":203}],153:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Additive = function (x) {
    return x;
};
var showAdditive = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Additive (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semigroupAdditive = function (dictSemiring) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude["+"](dictSemiring)(v)(v1);
        };
    });
};
var runAdditive = function (v) {
    return v;
};
var monoidAdditive = function (dictSemiring) {
    return new Data_Monoid.Monoid(function () {
        return semigroupAdditive(dictSemiring);
    }, Prelude.zero(dictSemiring));
};
var invariantAdditive = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorAdditive = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendAdditive = new Control_Extend.Extend(function () {
    return functorAdditive;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqAdditive = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordAdditive = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqAdditive(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadAdditive = new Control_Comonad.Comonad(function () {
    return extendAdditive;
}, runAdditive);
var applyAdditive = new Prelude.Apply(function () {
    return functorAdditive;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindAdditive = new Prelude.Bind(function () {
    return applyAdditive;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeAdditive = new Prelude.Applicative(function () {
    return applyAdditive;
}, Additive);
var monadAdditive = new Prelude.Monad(function () {
    return applicativeAdditive;
}, function () {
    return bindAdditive;
});
module.exports = {
    Additive: Additive, 
    runAdditive: runAdditive, 
    eqAdditive: eqAdditive, 
    ordAdditive: ordAdditive, 
    functorAdditive: functorAdditive, 
    applyAdditive: applyAdditive, 
    applicativeAdditive: applicativeAdditive, 
    bindAdditive: bindAdditive, 
    monadAdditive: monadAdditive, 
    extendAdditive: extendAdditive, 
    comonadAdditive: comonadAdditive, 
    invariantAdditive: invariantAdditive, 
    showAdditive: showAdditive, 
    semigroupAdditive: semigroupAdditive, 
    monoidAdditive: monoidAdditive
};

},{"../Control.Comonad":32,"../Control.Extend":36,"../Data.Functor.Invariant":133,"../Data.Monoid":159,"../Prelude":203}],154:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Data_Monoid = require("../Data.Monoid");
var Conj = function (x) {
    return x;
};
var showConj = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Conj (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semiringConj = function (dictBooleanAlgebra) {
    return new Prelude.Semiring(function (v) {
        return function (v1) {
            return Prelude.conj(dictBooleanAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Prelude.disj(dictBooleanAlgebra)(v)(v1);
        };
    }, Prelude.bottom(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()), Prelude.top(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()));
};
var semigroupConj = function (dictBooleanAlgebra) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude.conj(dictBooleanAlgebra)(v)(v1);
        };
    });
};
var runConj = function (v) {
    return v;
};
var monoidConj = function (dictBooleanAlgebra) {
    return new Data_Monoid.Monoid(function () {
        return semigroupConj(dictBooleanAlgebra);
    }, Prelude.top(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()));
};
var functorConj = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendConj = new Control_Extend.Extend(function () {
    return functorConj;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqConj = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordConj = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqConj(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadConj = new Control_Comonad.Comonad(function () {
    return extendConj;
}, runConj);
var boundedConj = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(dictBounded), Prelude.top(dictBounded));
};
var applyConj = new Prelude.Apply(function () {
    return functorConj;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindConj = new Prelude.Bind(function () {
    return applyConj;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeConj = new Prelude.Applicative(function () {
    return applyConj;
}, Conj);
var monadConj = new Prelude.Monad(function () {
    return applicativeConj;
}, function () {
    return bindConj;
});
module.exports = {
    Conj: Conj, 
    runConj: runConj, 
    eqConj: eqConj, 
    ordConj: ordConj, 
    boundedConj: boundedConj, 
    functorConj: functorConj, 
    applyConj: applyConj, 
    applicativeConj: applicativeConj, 
    bindConj: bindConj, 
    monadConj: monadConj, 
    extendConj: extendConj, 
    comonadConj: comonadConj, 
    showConj: showConj, 
    semigroupConj: semigroupConj, 
    monoidConj: monoidConj, 
    semiringConj: semiringConj
};

},{"../Control.Comonad":32,"../Control.Extend":36,"../Data.Monoid":159,"../Prelude":203}],155:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Data_Monoid = require("../Data.Monoid");
var Disj = function (x) {
    return x;
};
var showDisj = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Disj (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semiringDisj = function (dictBooleanAlgebra) {
    return new Prelude.Semiring(function (v) {
        return function (v1) {
            return Prelude.disj(dictBooleanAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Prelude.conj(dictBooleanAlgebra)(v)(v1);
        };
    }, Prelude.top(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()), Prelude.bottom(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()));
};
var semigroupDisj = function (dictBooleanAlgebra) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude.disj(dictBooleanAlgebra)(v)(v1);
        };
    });
};
var runDisj = function (v) {
    return v;
};
var monoidDisj = function (dictBooleanAlgebra) {
    return new Data_Monoid.Monoid(function () {
        return semigroupDisj(dictBooleanAlgebra);
    }, Prelude.bottom(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()));
};
var functorDisj = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendDisj = new Control_Extend.Extend(function () {
    return functorDisj;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqDisj = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordDisj = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqDisj(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadDisj = new Control_Comonad.Comonad(function () {
    return extendDisj;
}, runDisj);
var boundedDisj = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(dictBounded), Prelude.top(dictBounded));
};
var applyDisj = new Prelude.Apply(function () {
    return functorDisj;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindDisj = new Prelude.Bind(function () {
    return applyDisj;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeDisj = new Prelude.Applicative(function () {
    return applyDisj;
}, Disj);
var monadDisj = new Prelude.Monad(function () {
    return applicativeDisj;
}, function () {
    return bindDisj;
});
module.exports = {
    Disj: Disj, 
    runDisj: runDisj, 
    eqDisj: eqDisj, 
    ordDisj: ordDisj, 
    boundedDisj: boundedDisj, 
    functorDisj: functorDisj, 
    applyDisj: applyDisj, 
    applicativeDisj: applicativeDisj, 
    bindDisj: bindDisj, 
    monadDisj: monadDisj, 
    extendDisj: extendDisj, 
    comonadDisj: comonadDisj, 
    showDisj: showDisj, 
    semigroupDisj: semigroupDisj, 
    monoidDisj: monoidDisj, 
    semiringDisj: semiringDisj
};

},{"../Control.Comonad":32,"../Control.Extend":36,"../Data.Monoid":159,"../Prelude":203}],156:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Dual = function (x) {
    return x;
};
var showDual = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Dual (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semigroupDual = function (dictSemigroup) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude["<>"](dictSemigroup)(v1)(v);
        };
    });
};
var runDual = function (v) {
    return v;
};
var monoidDual = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupDual(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Data_Monoid.mempty(dictMonoid));
};
var invariantDual = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorDual = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendDual = new Control_Extend.Extend(function () {
    return functorDual;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqDual = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordDual = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqDual(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadDual = new Control_Comonad.Comonad(function () {
    return extendDual;
}, runDual);
var applyDual = new Prelude.Apply(function () {
    return functorDual;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindDual = new Prelude.Bind(function () {
    return applyDual;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeDual = new Prelude.Applicative(function () {
    return applyDual;
}, Dual);
var monadDual = new Prelude.Monad(function () {
    return applicativeDual;
}, function () {
    return bindDual;
});
module.exports = {
    Dual: Dual, 
    runDual: runDual, 
    eqDual: eqDual, 
    ordDual: ordDual, 
    functorDual: functorDual, 
    applyDual: applyDual, 
    applicativeDual: applicativeDual, 
    bindDual: bindDual, 
    monadDual: monadDual, 
    extendDual: extendDual, 
    comonadDual: comonadDual, 
    invariantDual: invariantDual, 
    showDual: showDual, 
    semigroupDual: semigroupDual, 
    monoidDual: monoidDual
};

},{"../Control.Comonad":32,"../Control.Extend":36,"../Data.Functor.Invariant":133,"../Data.Monoid":159,"../Prelude":203}],157:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Endo = function (x) {
    return x;
};
var semigroupEndo = new Prelude.Semigroup(function (v) {
    return function (v1) {
        return function ($10) {
            return v(v1($10));
        };
    };
});
var runEndo = function (v) {
    return v;
};
var monoidEndo = new Data_Monoid.Monoid(function () {
    return semigroupEndo;
}, Prelude.id(Prelude.categoryFn));
var invariantEndo = new Data_Functor_Invariant.Invariant(function (ab) {
    return function (ba) {
        return function (v) {
            return function ($11) {
                return ab(v(ba($11)));
            };
        };
    };
});
module.exports = {
    Endo: Endo, 
    runEndo: runEndo, 
    invariantEndo: invariantEndo, 
    semigroupEndo: semigroupEndo, 
    monoidEndo: monoidEndo
};

},{"../Data.Functor.Invariant":133,"../Data.Monoid":159,"../Prelude":203}],158:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Multiplicative = function (x) {
    return x;
};
var showMultiplicative = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Multiplicative (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semigroupMultiplicative = function (dictSemiring) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude["*"](dictSemiring)(v)(v1);
        };
    });
};
var runMultiplicative = function (v) {
    return v;
};
var monoidMultiplicative = function (dictSemiring) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMultiplicative(dictSemiring);
    }, Prelude.one(dictSemiring));
};
var invariantMultiplicative = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorMultiplicative = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendMultiplicative = new Control_Extend.Extend(function () {
    return functorMultiplicative;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqMultiplicative = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordMultiplicative = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqMultiplicative(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadMultiplicative = new Control_Comonad.Comonad(function () {
    return extendMultiplicative;
}, runMultiplicative);
var applyMultiplicative = new Prelude.Apply(function () {
    return functorMultiplicative;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindMultiplicative = new Prelude.Bind(function () {
    return applyMultiplicative;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeMultiplicative = new Prelude.Applicative(function () {
    return applyMultiplicative;
}, Multiplicative);
var monadMultiplicative = new Prelude.Monad(function () {
    return applicativeMultiplicative;
}, function () {
    return bindMultiplicative;
});
module.exports = {
    Multiplicative: Multiplicative, 
    runMultiplicative: runMultiplicative, 
    eqMultiplicative: eqMultiplicative, 
    ordMultiplicative: ordMultiplicative, 
    functorMultiplicative: functorMultiplicative, 
    applyMultiplicative: applyMultiplicative, 
    applicativeMultiplicative: applicativeMultiplicative, 
    bindMultiplicative: bindMultiplicative, 
    monadMultiplicative: monadMultiplicative, 
    extendMultiplicative: extendMultiplicative, 
    comonadMultiplicative: comonadMultiplicative, 
    invariantMultiplicative: invariantMultiplicative, 
    showMultiplicative: showMultiplicative, 
    semigroupMultiplicative: semigroupMultiplicative, 
    monoidMultiplicative: monoidMultiplicative
};

},{"../Control.Comonad":32,"../Control.Extend":36,"../Data.Functor.Invariant":133,"../Data.Monoid":159,"../Prelude":203}],159:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Monoid = function (__superclass_Prelude$dotSemigroup_0, mempty) {
    this["__superclass_Prelude.Semigroup_0"] = __superclass_Prelude$dotSemigroup_0;
    this.mempty = mempty;
};
var monoidUnit = new Monoid(function () {
    return Prelude.semigroupUnit;
}, Prelude.unit);
var monoidString = new Monoid(function () {
    return Prelude.semigroupString;
}, "");
var monoidArray = new Monoid(function () {
    return Prelude.semigroupArray;
}, [  ]);
var mempty = function (dict) {
    return dict.mempty;
};
var monoidFn = function (dictMonoid) {
    return new Monoid(function () {
        return Prelude.semigroupFn(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Prelude["const"](mempty(dictMonoid)));
};
module.exports = {
    Monoid: Monoid, 
    mempty: mempty, 
    monoidUnit: monoidUnit, 
    monoidFn: monoidFn, 
    monoidString: monoidString, 
    monoidArray: monoidArray
};

},{"../Prelude":203}],160:[function(require,module,exports){
arguments[4][98][0].apply(exports,arguments)
},{"dup":98}],161:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Nullable

exports["null"] = null;

exports.nullable = function(a, r, f) {
    return a == null ? r : f(a);
};

exports.notNull = function(x) {
    return x;
}; 

},{}],162:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Function = require("../Data.Function");
var toNullable = Data_Maybe.maybe($foreign["null"])($foreign.notNull);
var toMaybe = function (n) {
    return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
};
var showNullable = function (dictShow) {
    return new Prelude.Show(function (n) {
        var $3 = toMaybe(n);
        if ($3 instanceof Data_Maybe.Nothing) {
            return "null";
        };
        if ($3 instanceof Data_Maybe.Just) {
            return Prelude.show(dictShow)($3.value0);
        };
        throw new Error("Failed pattern match at Data.Nullable line 38, column 12 - line 42, column 1: " + [ $3.constructor.name ]);
    });
};
var eqNullable = function (dictEq) {
    return new Prelude.Eq(Data_Function.on(Prelude.eq(Data_Maybe.eqMaybe(dictEq)))(toMaybe));
};
var ordNullable = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqNullable(dictOrd["__superclass_Prelude.Eq_0"]());
    }, Data_Function.on(Prelude.compare(Data_Maybe.ordMaybe(dictOrd)))(toMaybe));
};
module.exports = {
    toNullable: toNullable, 
    toMaybe: toMaybe, 
    showNullable: showNullable, 
    eqNullable: eqNullable, 
    ordNullable: ordNullable
};

},{"../Data.Function":130,"../Data.Maybe":152,"../Prelude":203,"./foreign":161}],163:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Profunctor = require("../Data.Profunctor");
var Choice = function (__superclass_Data$dotProfunctor$dotProfunctor_0, left, right) {
    this["__superclass_Data.Profunctor.Profunctor_0"] = __superclass_Data$dotProfunctor$dotProfunctor_0;
    this.left = left;
    this.right = right;
};
var right = function (dict) {
    return dict.right;
};
var left = function (dict) {
    return dict.left;
};
var $plus$plus$plus = function (dictCategory) {
    return function (dictChoice) {
        return function (l) {
            return function (r) {
                return Prelude[">>>"](dictCategory["__superclass_Prelude.Semigroupoid_0"]())(left(dictChoice)(l))(right(dictChoice)(r));
            };
        };
    };
};
var $bar$bar$bar = function (dictCategory) {
    return function (dictChoice) {
        return function (l) {
            return function (r) {
                var join = Data_Profunctor.dimap(dictChoice["__superclass_Data.Profunctor.Profunctor_0"]())(Data_Either.either(Prelude.id(Prelude.categoryFn))(Prelude.id(Prelude.categoryFn)))(Prelude.id(Prelude.categoryFn))(Prelude.id(dictCategory));
                return Prelude[">>>"](dictCategory["__superclass_Prelude.Semigroupoid_0"]())($plus$plus$plus(dictCategory)(dictChoice)(l)(r))(join);
            };
        };
    };
};
var choiceFn = new Choice(function () {
    return Data_Profunctor.profunctorFn;
}, function (v) {
    return function (v1) {
        if (v1 instanceof Data_Either.Left) {
            return Data_Either.Left.create(v(v1.value0));
        };
        if (v1 instanceof Data_Either.Right) {
            return new Data_Either.Right(v1.value0);
        };
        throw new Error("Failed pattern match at Data.Profunctor.Choice line 18, column 3 - line 19, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
}, Prelude["<$>"](Data_Either.functorEither));
module.exports = {
    Choice: Choice, 
    "|||": $bar$bar$bar, 
    "+++": $plus$plus$plus, 
    right: right, 
    left: left, 
    choiceFn: choiceFn
};

},{"../Data.Either":114,"../Data.Profunctor":164,"../Prelude":203}],164:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Profunctor = function (dimap) {
    this.dimap = dimap;
};
var profunctorFn = new Profunctor(function (a2b) {
    return function (c2d) {
        return function (b2c) {
            return function ($4) {
                return c2d(b2c(a2b($4)));
            };
        };
    };
});
var dimap = function (dict) {
    return dict.dimap;
};
var lmap = function (dictProfunctor) {
    return function (a2b) {
        return dimap(dictProfunctor)(a2b)(Prelude.id(Prelude.categoryFn));
    };
};
var rmap = function (dictProfunctor) {
    return function (b2c) {
        return dimap(dictProfunctor)(Prelude.id(Prelude.categoryFn))(b2c);
    };
};
var arr = function (dictCategory) {
    return function (dictProfunctor) {
        return function (f) {
            return rmap(dictProfunctor)(f)(Prelude.id(dictCategory));
        };
    };
};
module.exports = {
    Profunctor: Profunctor, 
    arr: arr, 
    rmap: rmap, 
    lmap: lmap, 
    dimap: dimap, 
    profunctorFn: profunctorFn
};

},{"../Prelude":203}],165:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.String.Unsafe

exports.charCodeAt = function (i) {
  return function (s) {
    if (i >= 0 && i < s.length) return s.charCodeAt(i);
    throw new Error("Data.String.Unsafe.charCodeAt: Invalid index.");
  };
};

exports.charAt = function (i) {
  return function (s) {
    if (i >= 0 && i < s.length) return s.charAt(i);
    throw new Error("Data.String.Unsafe.charAt: Invalid index.");
  };
};

exports.char = function (s) {
  if (s.length === 1) return s.charAt(0);
  throw new Error("Data.String.Unsafe.char: Expected string of length 1.");
};

},{}],166:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
module.exports = {
    charCodeAt: $foreign.charCodeAt, 
    charAt: $foreign.charAt, 
    "char": $foreign["char"]
};

},{"./foreign":165}],167:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.String

exports._charAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
      };
    };
  };
};

exports._charCodeAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.charCodeAt(i)) : nothing;
      };
    };
  };
};

exports._toChar = function (just) {
  return function (nothing) {
    return function (s) {
      return s.length === 1 ? just(s) : nothing;
    };
  };
};

exports.fromCharArray = function (a) {
  return a.join("");
};

exports._indexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.indexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

exports["_indexOf'"] = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.indexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

exports._lastIndexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.lastIndexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

exports["_lastIndexOf'"] = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.lastIndexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

exports.length = function (s) {
  return s.length;
};

exports._localeCompare = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (s1) {
        return function (s2) {
          var result = s1.localeCompare(s2);
          return result < 0 ? lt : result > 0 ? gt : eq;
        };
      };
    };
  };
};

exports.replace = function (s1) {
  return function (s2) {
    return function (s3) {
      return s3.replace(s1, s2);
    };
  };
};

exports.take = function (n) {
  return function (s) {
    return s.substr(0, n);
  };
};

exports.drop = function (n) {
  return function (s) {
    return s.substr(n);
  };
};

exports.count = function (p) {
  return function (s) {
    for (var i = 0; i < s.length && p(s.charAt(i)); i++); {}
    return i;
  };
};

exports.split = function (sep) {
  return function (s) {
    return s.split(sep);
  };
};

exports.toCharArray = function (s) {
  return s.split("");
};

exports.toLower = function (s) {
  return s.toLowerCase();
};

exports.toUpper = function (s) {
  return s.toUpperCase();
};

exports.trim = function (s) {
  return s.trim();
};

exports.joinWith = function (s) {
  return function (xs) {
    return xs.join(s);
  };
};

},{}],168:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Char = require("../Data.Char");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_String_Unsafe = require("../Data.String.Unsafe");
var uncons = function (v) {
    if (v === "") {
        return Data_Maybe.Nothing.value;
    };
    return new Data_Maybe.Just({
        head: Data_String_Unsafe.charAt(0)(v), 
        tail: $foreign.drop(1)(v)
    });
};
var toChar = $foreign._toChar(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var takeWhile = function (p) {
    return function (s) {
        return $foreign.take($foreign.count(p)(s))(s);
    };
};
var $$null = function (s) {
    return $foreign.length(s) === 0;
};
var localeCompare = $foreign._localeCompare(Prelude.LT.value)(Prelude.EQ.value)(Prelude.GT.value);
var lastIndexOf$prime = $foreign["_lastIndexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var lastIndexOf = $foreign._lastIndexOf(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripSuffix = function (suffix) {
    return function (str) {
        var $2 = lastIndexOf(suffix)(str);
        if ($2 instanceof Data_Maybe.Just && $2.value0 === $foreign.length(str) - $foreign.length(suffix)) {
            return Data_Maybe.Just.create($foreign.take($2.value0)(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var indexOf$prime = $foreign["_indexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var indexOf = $foreign._indexOf(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripPrefix = function (prefix) {
    return function (str) {
        var $4 = indexOf(prefix)(str);
        if ($4 instanceof Data_Maybe.Just && $4.value0 === 0) {
            return Data_Maybe.Just.create($foreign.drop($foreign.length(prefix))(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var fromChar = Data_Char.toString;
var singleton = fromChar;
var dropWhile = function (p) {
    return function (s) {
        return $foreign.drop($foreign.count(p)(s))(s);
    };
};
var contains = function (x) {
    return function (s) {
        return Data_Maybe.isJust(indexOf(x)(s));
    };
};
var charCodeAt = $foreign._charCodeAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var charAt = $foreign._charAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
module.exports = {
    stripSuffix: stripSuffix, 
    stripPrefix: stripPrefix, 
    dropWhile: dropWhile, 
    takeWhile: takeWhile, 
    localeCompare: localeCompare, 
    singleton: singleton, 
    uncons: uncons, 
    "null": $$null, 
    "lastIndexOf'": lastIndexOf$prime, 
    lastIndexOf: lastIndexOf, 
    "indexOf'": indexOf$prime, 
    indexOf: indexOf, 
    contains: contains, 
    toChar: toChar, 
    fromChar: fromChar, 
    charCodeAt: charCodeAt, 
    charAt: charAt, 
    joinWith: $foreign.joinWith, 
    trim: $foreign.trim, 
    toUpper: $foreign.toUpper, 
    toLower: $foreign.toLower, 
    toCharArray: $foreign.toCharArray, 
    split: $foreign.split, 
    drop: $foreign.drop, 
    take: $foreign.take, 
    count: $foreign.count, 
    replace: $foreign.replace, 
    length: $foreign.length, 
    fromCharArray: $foreign.fromCharArray
};

},{"../Data.Char":109,"../Data.Maybe":152,"../Data.Monoid":159,"../Data.String.Unsafe":166,"../Prelude":203,"./foreign":167}],169:[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Traversable

// jshint maxparams: 3

exports.traverseArrayImpl = function () {
  function Cont (fn) {
    this.fn = fn;
  }

  var emptyList = {};

  var ConsCell = function (head, tail) {
    this.head = head;
    this.tail = tail;
  };

  function consList (x) {
    return function (xs) {
      return new ConsCell(x, xs);
    };
  }

  function listToArray (list) {
    var arr = [];
    while (list !== emptyList) {
      arr.push(list.head);
      list = list.tail;
    }
    return arr;
  }

  return function (apply) {
    return function (map) {
      return function (pure) {
        return function (f) {
          var buildFrom = function (x, ys) {
            return apply(map(consList)(f(x)))(ys);
          };

          var go = function (acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last = xs[currentLen - 1];
              return new Cont(function () {
                return go(buildFrom(last, acc), currentLen - 1, xs);
              });
            }
          };

          return function (array) {
            var result = go(pure(emptyList), array.length, array);
            while (result instanceof Cont) {
              result = result.fn();
            }

            return map(listToArray)(result);
          };
        };
      };
    };
  };
}();

},{}],170:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Foldable = require("../Data.Foldable");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_First = require("../Data.Maybe.First");
var Data_Maybe_Last = require("../Data.Maybe.Last");
var Data_Monoid_Additive = require("../Data.Monoid.Additive");
var Data_Monoid_Conj = require("../Data.Monoid.Conj");
var Data_Monoid_Disj = require("../Data.Monoid.Disj");
var Data_Monoid_Dual = require("../Data.Monoid.Dual");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative");
var StateL = function (x) {
    return x;
};
var StateR = function (x) {
    return x;
};
var Traversable = function (__superclass_Data$dotFoldable$dotFoldable_1, __superclass_Prelude$dotFunctor_0, sequence, traverse) {
    this["__superclass_Data.Foldable.Foldable_1"] = __superclass_Data$dotFoldable$dotFoldable_1;
    this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    this.sequence = sequence;
    this.traverse = traverse;
};
var traverse = function (dict) {
    return dict.traverse;
};
var traversableMultiplicative = new Traversable(function () {
    return Data_Foldable.foldableMultiplicative;
}, function () {
    return Data_Monoid_Multiplicative.functorMultiplicative;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Multiplicative.Multiplicative)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Multiplicative.Multiplicative)(f(v));
        };
    };
});
var traversableMaybe = new Traversable(function () {
    return Data_Foldable.foldableMaybe;
}, function () {
    return Data_Maybe.functorMaybe;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Prelude.pure(dictApplicative)(Data_Maybe.Nothing.value);
        };
        if (v instanceof Data_Maybe.Just) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe.Just.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Traversable line 79, column 3 - line 80, column 3: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return Prelude.pure(dictApplicative)(Data_Maybe.Nothing.value);
            };
            if (v1 instanceof Data_Maybe.Just) {
                return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe.Just.create)(v(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Traversable line 77, column 3 - line 78, column 3: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var traversableDual = new Traversable(function () {
    return Data_Foldable.foldableDual;
}, function () {
    return Data_Monoid_Dual.functorDual;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Dual.Dual)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Dual.Dual)(f(v));
        };
    };
});
var traversableDisj = new Traversable(function () {
    return Data_Foldable.foldableDisj;
}, function () {
    return Data_Monoid_Disj.functorDisj;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Disj.Disj)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Disj.Disj)(f(v));
        };
    };
});
var traversableConj = new Traversable(function () {
    return Data_Foldable.foldableConj;
}, function () {
    return Data_Monoid_Conj.functorConj;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Conj.Conj)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Conj.Conj)(f(v));
        };
    };
});
var traversableAdditive = new Traversable(function () {
    return Data_Foldable.foldableAdditive;
}, function () {
    return Data_Monoid_Additive.functorAdditive;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Additive.Additive)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Additive.Additive)(f(v));
        };
    };
});
var stateR = function (v) {
    return v;
};
var stateL = function (v) {
    return v;
};
var sequenceDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return function (tma) {
            return traverse(dictTraversable)(dictApplicative)(Prelude.id(Prelude.categoryFn))(tma);
        };
    };
};
var traversableArray = new Traversable(function () {
    return Data_Foldable.foldableArray;
}, function () {
    return Prelude.functorArray;
}, function (dictApplicative) {
    return sequenceDefault(traversableArray)(dictApplicative);
}, function (dictApplicative) {
    return $foreign.traverseArrayImpl(Prelude.apply(dictApplicative["__superclass_Prelude.Apply_0"]()))(Prelude.map((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]()))(Prelude.pure(dictApplicative));
});
var sequence = function (dict) {
    return dict.sequence;
};
var traversableFirst = new Traversable(function () {
    return Data_Foldable.foldableFirst;
}, function () {
    return Data_Maybe_First.functorFirst;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe_First.First)(sequence(traversableMaybe)(dictApplicative)(v));
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe_First.First)(traverse(traversableMaybe)(dictApplicative)(f)(v));
        };
    };
});
var traversableLast = new Traversable(function () {
    return Data_Foldable.foldableLast;
}, function () {
    return Data_Maybe_Last.functorLast;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe_Last.Last)(sequence(traversableMaybe)(dictApplicative)(v));
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe_Last.Last)(traverse(traversableMaybe)(dictApplicative)(f)(v));
        };
    };
});
var traverseDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (ta) {
                return sequence(dictTraversable)(dictApplicative)(Prelude.map(dictTraversable["__superclass_Prelude.Functor_0"]())(f)(ta));
            };
        };
    };
};
var functorStateR = new Prelude.Functor(function (f) {
    return function (k) {
        return function (s) {
            var $75 = stateR(k)(s);
            return {
                accum: $75.accum, 
                value: f($75.value)
            };
        };
    };
});
var functorStateL = new Prelude.Functor(function (f) {
    return function (k) {
        return function (s) {
            var $78 = stateL(k)(s);
            return {
                accum: $78.accum, 
                value: f($78.value)
            };
        };
    };
});
var $$for = function (dictApplicative) {
    return function (dictTraversable) {
        return function (x) {
            return function (f) {
                return traverse(dictTraversable)(dictApplicative)(f)(x);
            };
        };
    };
};
var applyStateR = new Prelude.Apply(function () {
    return functorStateR;
}, function (f) {
    return function (x) {
        return function (s) {
            var $81 = stateR(x)(s);
            var $82 = stateR(f)($81.accum);
            return {
                accum: $82.accum, 
                value: $82.value($81.value)
            };
        };
    };
});
var applyStateL = new Prelude.Apply(function () {
    return functorStateL;
}, function (f) {
    return function (x) {
        return function (s) {
            var $87 = stateL(f)(s);
            var $88 = stateL(x)($87.accum);
            return {
                accum: $88.accum, 
                value: $87.value($88.value)
            };
        };
    };
});
var applicativeStateR = new Prelude.Applicative(function () {
    return applyStateR;
}, function (a) {
    return function (s) {
        return {
            accum: s, 
            value: a
        };
    };
});
var mapAccumR = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return stateR(traverse(dictTraversable)(applicativeStateR)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanr = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumR(dictTraversable)(function (b) {
                    return function (a) {
                        var b$prime = f(a)(b);
                        return {
                            accum: b$prime, 
                            value: b$prime
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
var applicativeStateL = new Prelude.Applicative(function () {
    return applyStateL;
}, function (a) {
    return function (s) {
        return {
            accum: s, 
            value: a
        };
    };
});
var mapAccumL = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return stateL(traverse(dictTraversable)(applicativeStateL)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanl = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumL(dictTraversable)(function (b) {
                    return function (a) {
                        var b$prime = f(b)(a);
                        return {
                            accum: b$prime, 
                            value: b$prime
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
module.exports = {
    Traversable: Traversable, 
    mapAccumR: mapAccumR, 
    mapAccumL: mapAccumL, 
    scanr: scanr, 
    scanl: scanl, 
    "for": $$for, 
    sequenceDefault: sequenceDefault, 
    traverseDefault: traverseDefault, 
    sequence: sequence, 
    traverse: traverse, 
    traversableArray: traversableArray, 
    traversableMaybe: traversableMaybe, 
    traversableFirst: traversableFirst, 
    traversableLast: traversableLast, 
    traversableAdditive: traversableAdditive, 
    traversableDual: traversableDual, 
    traversableConj: traversableConj, 
    traversableDisj: traversableDisj, 
    traversableMultiplicative: traversableMultiplicative
};

},{"../Data.Foldable":120,"../Data.Maybe":152,"../Data.Maybe.First":148,"../Data.Maybe.Last":149,"../Data.Monoid.Additive":153,"../Data.Monoid.Conj":154,"../Data.Monoid.Disj":155,"../Data.Monoid.Dual":156,"../Data.Monoid.Multiplicative":158,"../Prelude":203,"./foreign":169}],171:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Biapplicative = require("../Control.Biapplicative");
var Control_Biapply = require("../Control.Biapply");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Lazy = require("../Control.Lazy");
var Data_Bifoldable = require("../Data.Bifoldable");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Bitraversable = require("../Data.Bitraversable");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_First = require("../Data.Maybe.First");
var Data_Monoid = require("../Data.Monoid");
var Data_Traversable = require("../Data.Traversable");
var Tuple = (function () {
    function Tuple(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Tuple.create = function (value0) {
        return function (value1) {
            return new Tuple(value0, value1);
        };
    };
    return Tuple;
})();
var uncurry = function (f) {
    return function (v) {
        return f(v.value0)(v.value1);
    };
};
var swap = function (v) {
    return new Tuple(v.value1, v.value0);
};
var snd = function (v) {
    return v.value1;
};
var showTuple = function (dictShow) {
    return function (dictShow1) {
        return new Prelude.Show(function (v) {
            return "Tuple (" + (Prelude.show(dictShow)(v.value0) + (") (" + (Prelude.show(dictShow1)(v.value1) + ")")));
        });
    };
};
var semiringTuple = function (dictSemiring) {
    return function (dictSemiring1) {
        return new Prelude.Semiring(function (v) {
            return function (v1) {
                return new Tuple(Prelude.add(dictSemiring)(v.value0)(v1.value0), Prelude.add(dictSemiring1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.mul(dictSemiring)(v.value0)(v1.value0), Prelude.mul(dictSemiring1)(v.value1)(v1.value1));
            };
        }, new Tuple(Prelude.one(dictSemiring), Prelude.one(dictSemiring1)), new Tuple(Prelude.zero(dictSemiring), Prelude.zero(dictSemiring1)));
    };
};
var semigroupoidTuple = new Prelude.Semigroupoid(function (v) {
    return function (v1) {
        return new Tuple(v1.value0, v.value1);
    };
});
var semigroupTuple = function (dictSemigroup) {
    return function (dictSemigroup1) {
        return new Prelude.Semigroup(function (v) {
            return function (v1) {
                return new Tuple(Prelude["<>"](dictSemigroup)(v.value0)(v1.value0), Prelude["<>"](dictSemigroup1)(v.value1)(v1.value1));
            };
        });
    };
};
var ringTuple = function (dictRing) {
    return function (dictRing1) {
        return new Prelude.Ring(function () {
            return semiringTuple(dictRing["__superclass_Prelude.Semiring_0"]())(dictRing1["__superclass_Prelude.Semiring_0"]());
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.sub(dictRing)(v.value0)(v1.value0), Prelude.sub(dictRing1)(v.value1)(v1.value1));
            };
        });
    };
};
var monoidTuple = function (dictMonoid) {
    return function (dictMonoid1) {
        return new Data_Monoid.Monoid(function () {
            return semigroupTuple(dictMonoid["__superclass_Prelude.Semigroup_0"]())(dictMonoid1["__superclass_Prelude.Semigroup_0"]());
        }, new Tuple(Data_Monoid.mempty(dictMonoid), Data_Monoid.mempty(dictMonoid1)));
    };
};
var moduloSemiringTuple = function (dictModuloSemiring) {
    return function (dictModuloSemiring1) {
        return new Prelude.ModuloSemiring(function () {
            return semiringTuple(dictModuloSemiring["__superclass_Prelude.Semiring_0"]())(dictModuloSemiring1["__superclass_Prelude.Semiring_0"]());
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.div(dictModuloSemiring)(v.value0)(v1.value0), Prelude.div(dictModuloSemiring1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.mod(dictModuloSemiring)(v.value0)(v1.value0), Prelude.mod(dictModuloSemiring1)(v.value1)(v1.value1));
            };
        });
    };
};
var lookup = function (dictFoldable) {
    return function (dictEq) {
        return function (a) {
            return function (f) {
                return Data_Maybe_First.runFirst(Data_Foldable.foldMap(dictFoldable)(Data_Maybe_First.monoidFirst)(function (v) {
                    var $145 = Prelude["=="](dictEq)(a)(v.value0);
                    if ($145) {
                        return new Data_Maybe.Just(v.value1);
                    };
                    if (!$145) {
                        return Data_Maybe.Nothing.value;
                    };
                    throw new Error("Failed pattern match at Data.Tuple line 174, column 58 - line 174, column 93: " + [ $145.constructor.name ]);
                })(f));
            };
        };
    };
};
var functorTuple = new Prelude.Functor(function (f) {
    return function (v) {
        return new Tuple(v.value0, f(v.value1));
    };
});
var invariantTuple = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorTuple));
var fst = function (v) {
    return v.value0;
};
var lazyTuple = function (dictLazy) {
    return function (dictLazy1) {
        return new Control_Lazy.Lazy(function (f) {
            return new Tuple(Control_Lazy.defer(dictLazy)(function (v) {
                return fst(f(Prelude.unit));
            }), Control_Lazy.defer(dictLazy1)(function (v) {
                return snd(f(Prelude.unit));
            }));
        });
    };
};
var foldableTuple = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v.value1)(z);
        };
    };
});
var traversableTuple = new Data_Traversable.Traversable(function () {
    return foldableTuple;
}, function () {
    return functorTuple;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Tuple.create(v.value0))(v.value1);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Tuple.create(v.value0))(f(v.value1));
        };
    };
});
var extendTuple = new Control_Extend.Extend(function () {
    return functorTuple;
}, function (f) {
    return function (v) {
        return new Tuple(v.value0, f(v));
    };
});
var eqTuple = function (dictEq) {
    return function (dictEq1) {
        return new Prelude.Eq(function (v) {
            return function (v1) {
                return Prelude["=="](dictEq)(v.value0)(v1.value0) && Prelude["=="](dictEq1)(v.value1)(v1.value1);
            };
        });
    };
};
var ordTuple = function (dictOrd) {
    return function (dictOrd1) {
        return new Prelude.Ord(function () {
            return eqTuple(dictOrd["__superclass_Prelude.Eq_0"]())(dictOrd1["__superclass_Prelude.Eq_0"]());
        }, function (v) {
            return function (v1) {
                var $190 = Prelude.compare(dictOrd)(v.value0)(v1.value0);
                if ($190 instanceof Prelude.EQ) {
                    return Prelude.compare(dictOrd1)(v.value1)(v1.value1);
                };
                return $190;
            };
        });
    };
};
var divisionRingTuple = function (dictDivisionRing) {
    return function (dictDivisionRing1) {
        return new Prelude.DivisionRing(function () {
            return moduloSemiringTuple(dictDivisionRing["__superclass_Prelude.ModuloSemiring_1"]())(dictDivisionRing1["__superclass_Prelude.ModuloSemiring_1"]());
        }, function () {
            return ringTuple(dictDivisionRing["__superclass_Prelude.Ring_0"]())(dictDivisionRing1["__superclass_Prelude.Ring_0"]());
        });
    };
};
var numTuple = function (dictNum) {
    return function (dictNum1) {
        return new Prelude.Num(function () {
            return divisionRingTuple(dictNum["__superclass_Prelude.DivisionRing_0"]())(dictNum1["__superclass_Prelude.DivisionRing_0"]());
        });
    };
};
var curry = function (f) {
    return function (a) {
        return function (b) {
            return f(new Tuple(a, b));
        };
    };
};
var comonadTuple = new Control_Comonad.Comonad(function () {
    return extendTuple;
}, snd);
var boundedTuple = function (dictBounded) {
    return function (dictBounded1) {
        return new Prelude.Bounded(new Tuple(Prelude.bottom(dictBounded), Prelude.bottom(dictBounded1)), new Tuple(Prelude.top(dictBounded), Prelude.top(dictBounded1)));
    };
};
var boundedOrdTuple = function (dictBoundedOrd) {
    return function (dictBoundedOrd1) {
        return new Prelude.BoundedOrd(function () {
            return boundedTuple(dictBoundedOrd["__superclass_Prelude.Bounded_0"]())(dictBoundedOrd1["__superclass_Prelude.Bounded_0"]());
        }, function () {
            return ordTuple(dictBoundedOrd["__superclass_Prelude.Ord_1"]())(dictBoundedOrd1["__superclass_Prelude.Ord_1"]());
        });
    };
};
var booleanAlgebraTuple = function (dictBooleanAlgebra) {
    return function (dictBooleanAlgebra1) {
        return new Prelude.BooleanAlgebra(function () {
            return boundedTuple(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]())(dictBooleanAlgebra1["__superclass_Prelude.Bounded_0"]());
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.conj(dictBooleanAlgebra)(v.value0)(v1.value0), Prelude.conj(dictBooleanAlgebra1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.disj(dictBooleanAlgebra)(v.value0)(v1.value0), Prelude.disj(dictBooleanAlgebra1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return new Tuple(Prelude.not(dictBooleanAlgebra)(v.value0), Prelude.not(dictBooleanAlgebra1)(v.value1));
        });
    };
};
var bifunctorTuple = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (v) {
            return new Tuple(f(v.value0), g(v.value1));
        };
    };
});
var bifoldableTuple = new Data_Bifoldable.Bifoldable(function (dictMonoid) {
    return function (f) {
        return function (g) {
            return function (v) {
                return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(f(v.value0))(g(v.value1));
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                return g(f(z)(v.value0))(v.value1);
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                return f(v.value0)(g(v.value1)(z));
            };
        };
    };
});
var bitraversableTuple = new Data_Bitraversable.Bitraversable(function () {
    return bifoldableTuple;
}, function () {
    return bifunctorTuple;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Tuple.create)(v.value0))(v.value1);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (g) {
            return function (v) {
                return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Tuple.create)(f(v.value0)))(g(v.value1));
            };
        };
    };
});
var biapplyTuple = new Control_Biapply.Biapply(function () {
    return bifunctorTuple;
}, function (v) {
    return function (v1) {
        return new Tuple(v.value0(v1.value0), v.value1(v1.value1));
    };
});
var biapplicativeTuple = new Control_Biapplicative.Biapplicative(function () {
    return biapplyTuple;
}, Tuple.create);
var applyTuple = function (dictSemigroup) {
    return new Prelude.Apply(function () {
        return functorTuple;
    }, function (v) {
        return function (v1) {
            return new Tuple(Prelude["<>"](dictSemigroup)(v.value0)(v1.value0), v.value1(v1.value1));
        };
    });
};
var bindTuple = function (dictSemigroup) {
    return new Prelude.Bind(function () {
        return applyTuple(dictSemigroup);
    }, function (v) {
        return function (f) {
            var $254 = f(v.value1);
            return new Tuple(Prelude["<>"](dictSemigroup)(v.value0)($254.value0), $254.value1);
        };
    });
};
var applicativeTuple = function (dictMonoid) {
    return new Prelude.Applicative(function () {
        return applyTuple(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Tuple.create(Data_Monoid.mempty(dictMonoid)));
};
var monadTuple = function (dictMonoid) {
    return new Prelude.Monad(function () {
        return applicativeTuple(dictMonoid);
    }, function () {
        return bindTuple(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    });
};
module.exports = {
    Tuple: Tuple, 
    lookup: lookup, 
    swap: swap, 
    uncurry: uncurry, 
    curry: curry, 
    snd: snd, 
    fst: fst, 
    showTuple: showTuple, 
    eqTuple: eqTuple, 
    ordTuple: ordTuple, 
    boundedTuple: boundedTuple, 
    boundedOrdTuple: boundedOrdTuple, 
    semigroupoidTuple: semigroupoidTuple, 
    semigroupTuple: semigroupTuple, 
    monoidTuple: monoidTuple, 
    semiringTuple: semiringTuple, 
    moduloSemiringTuple: moduloSemiringTuple, 
    ringTuple: ringTuple, 
    divisionRingTuple: divisionRingTuple, 
    numTuple: numTuple, 
    booleanAlgebraTuple: booleanAlgebraTuple, 
    functorTuple: functorTuple, 
    invariantTuple: invariantTuple, 
    bifunctorTuple: bifunctorTuple, 
    applyTuple: applyTuple, 
    biapplyTuple: biapplyTuple, 
    applicativeTuple: applicativeTuple, 
    biapplicativeTuple: biapplicativeTuple, 
    bindTuple: bindTuple, 
    monadTuple: monadTuple, 
    extendTuple: extendTuple, 
    comonadTuple: comonadTuple, 
    lazyTuple: lazyTuple, 
    foldableTuple: foldableTuple, 
    bifoldableTuple: bifoldableTuple, 
    traversableTuple: traversableTuple, 
    bitraversableTuple: bitraversableTuple
};

},{"../Control.Biapplicative":29,"../Control.Biapply":30,"../Control.Comonad":32,"../Control.Extend":36,"../Control.Lazy":37,"../Data.Bifoldable":103,"../Data.Bifunctor":104,"../Data.Bitraversable":105,"../Data.Foldable":120,"../Data.Functor.Invariant":133,"../Data.Maybe":152,"../Data.Maybe.First":148,"../Data.Monoid":159,"../Data.Traversable":170,"../Prelude":203}],172:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Tuple = require("../Data.Tuple");
var Data_Array_ST = require("../Data.Array.ST");
var Data_Traversable = require("../Data.Traversable");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_ST = require("../Control.Monad.ST");
var Unfoldable = function (unfoldr) {
    this.unfoldr = unfoldr;
};
var unfoldr = function (dict) {
    return dict.unfoldr;
};
var unfoldableArray = new Unfoldable(function (f) {
    return function (b) {
        return Control_Monad_Eff.runPure(Data_Array_ST.runSTArray(function __do() {
            var v = Data_Array_ST.emptySTArray();
            var v1 = Control_Monad_ST.newSTRef(b)();
            (function () {
                while (!(function __do() {
                    var v2 = Control_Monad_ST.readSTRef(v1)();
                    var $12 = f(v2);
                    if ($12 instanceof Data_Maybe.Nothing) {
                        return true;
                    };
                    if ($12 instanceof Data_Maybe.Just) {
                        Data_Array_ST.pushSTArray(v)($12.value0.value0)();
                        Control_Monad_ST.writeSTRef(v1)($12.value0.value1)();
                        return false;
                    };
                    throw new Error("Failed pattern match at Data.Unfoldable line 35, column 7 - line 41, column 5: " + [ $12.constructor.name ]);
                })()) {

                };
                return {};
            })();
            return v;
        }));
    };
});
var replicate = function (dictUnfoldable) {
    return function (n) {
        return function (v) {
            var step = function (i) {
                var $16 = i <= 0;
                if ($16) {
                    return Data_Maybe.Nothing.value;
                };
                if (!$16) {
                    return new Data_Maybe.Just(new Data_Tuple.Tuple(v, i - 1));
                };
                throw new Error("Failed pattern match at Data.Unfoldable line 54, column 7 - line 58, column 1: " + [ $16.constructor.name ]);
            };
            return unfoldr(dictUnfoldable)(step)(n);
        };
    };
};
var replicateA = function (dictApplicative) {
    return function (dictUnfoldable) {
        return function (dictTraversable) {
            return function (n) {
                return function (m) {
                    return Data_Traversable.sequence(dictTraversable)(dictApplicative)(replicate(dictUnfoldable)(n)(m));
                };
            };
        };
    };
};
var singleton = function (dictUnfoldable) {
    return replicate(dictUnfoldable)(1);
};
var none = function (dictUnfoldable) {
    return unfoldr(dictUnfoldable)(Prelude["const"](Data_Maybe.Nothing.value))(Prelude.unit);
};
module.exports = {
    Unfoldable: Unfoldable, 
    singleton: singleton, 
    none: none, 
    replicateA: replicateA, 
    replicate: replicate, 
    unfoldr: unfoldr, 
    unfoldableArray: unfoldableArray
};

},{"../Control.Monad.Eff":54,"../Control.Monad.ST":67,"../Data.Array.ST":100,"../Data.Maybe":152,"../Data.Traversable":170,"../Data.Tuple":171,"../Prelude":203}],173:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Functor_Contravariant = require("../Data.Functor.Contravariant");
var Void = function (x) {
    return x;
};
var showVoid = new Prelude.Show(function (v) {
    return "Void";
});
var eqVoid = new Prelude.Eq(function (v) {
    return function (v1) {
        return true;
    };
});
var absurd = function (a) {
    var spin = function (__copy_v) {
        var v = __copy_v;
        tco: while (true) {
            var __tco_v = v;
            v = __tco_v;
            continue tco;
        };
    };
    return spin(a);
};
var coerce = function (dictContravariant) {
    return function (dictFunctor) {
        return function (a) {
            return Prelude["<$>"](dictFunctor)(absurd)(Data_Functor_Contravariant[">$<"](dictContravariant)(absurd)(a));
        };
    };
};
module.exports = {
    Void: Void, 
    absurd: absurd, 
    coerce: coerce, 
    eqVoid: eqVoid, 
    showVoid: showVoid
};

},{"../Data.Functor.Contravariant":131,"../Prelude":203}],174:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Either = require("../Data.Either");
var Data_Functor_Coproduct = require("../Data.Functor.Coproduct");
var Data_Injector = require("../Data.Injector");
var Data_Maybe = require("../Data.Maybe");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var ChildPath = (function () {
    function ChildPath(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    ChildPath.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new ChildPath(value0, value1, value2);
            };
        };
    };
    return ChildPath;
})();
var $colon$greater = compose;
var prjState = function (v) {
    return Data_Injector.prj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value0(dictChoice)(dictApplicative);
        };
    });
};
var prjSlot = function (v) {
    return Data_Injector.prj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value2(dictChoice)(dictApplicative);
        };
    });
};
var prjQuery = function (v) {
    return Data_Injector.prj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value1(dictChoice)(dictApplicative);
        };
    });
};
var injState = function (v) {
    return Data_Injector.inj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value0(dictChoice)(dictApplicative);
        };
    });
};
var injSlot = function (v) {
    return Data_Injector.inj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value2(dictChoice)(dictApplicative);
        };
    });
};
var injQuery = function (v) {
    return Data_Injector.inj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value1(dictChoice)(dictApplicative);
        };
    });
};
var cpR = new ChildPath(function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injRE(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injRC(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injRE(dictChoice)(dictApplicative);
    };
});
var cpL = new ChildPath(function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injLE(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injLC(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injLE(dictChoice)(dictApplicative);
    };
});
var cpI = new ChildPath(function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injI(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injI(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injI(dictChoice)(dictApplicative);
    };
});
var compose = function (v) {
    return function (v1) {
        return new ChildPath(function (dictChoice) {
            return function (dictApplicative) {
                return function ($76) {
                    return v.value0(dictChoice)(dictApplicative)(v1.value0(dictChoice)(dictApplicative)($76));
                };
            };
        }, function (dictChoice) {
            return function (dictApplicative) {
                return function ($77) {
                    return v.value1(dictChoice)(dictApplicative)(v1.value1(dictChoice)(dictApplicative)($77));
                };
            };
        }, function (dictChoice) {
            return function (dictApplicative) {
                return function ($78) {
                    return v.value2(dictChoice)(dictApplicative)(v1.value2(dictChoice)(dictApplicative)($78));
                };
            };
        });
    };
};
module.exports = {
    ChildPath: ChildPath, 
    prjSlot: prjSlot, 
    injSlot: injSlot, 
    prjQuery: prjQuery, 
    injQuery: injQuery, 
    prjState: prjState, 
    injState: injState, 
    cpI: cpI, 
    cpR: cpR, 
    cpL: cpL, 
    ":>": $colon$greater, 
    compose: compose
};

},{"../Data.Either":114,"../Data.Functor.Coproduct":132,"../Data.Injector":139,"../Data.Maybe":152,"../Data.NaturalTransformation":160,"../Prelude":203}],175:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Free = require("../Control.Monad.Free");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var Halogen_Query_HalogenF = require("../Halogen.Query.HalogenF");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var PostRender = (function () {
    function PostRender(value0) {
        this.value0 = value0;
    };
    PostRender.create = function (value0) {
        return new PostRender(value0);
    };
    return PostRender;
})();
var Finalized = (function () {
    function Finalized(value0) {
        this.value0 = value0;
    };
    Finalized.create = function (value0) {
        return new Finalized(value0);
    };
    return Finalized;
})();
var FinalizedF = (function () {
    function FinalizedF(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    FinalizedF.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new FinalizedF(value0, value1, value2);
            };
        };
    };
    return FinalizedF;
})();
var runFinalized = function (k) {
    return function (f) {
        var $6 = Unsafe_Coerce.unsafeCoerce(f);
        return k($6.value0)($6.value1)($6.value2);
    };
};
var lmapHook = function (v) {
    return function (v1) {
        if (v1 instanceof PostRender) {
            return new PostRender(v(v1.value0));
        };
        if (v1 instanceof Finalized) {
            return new Finalized(v1.value0);
        };
        throw new Error("Failed pattern match at Halogen.Component.Hook line 56, column 1 - line 57, column 1: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var finalized = function (e) {
    return function (s) {
        return function (i) {
            return Unsafe_Coerce.unsafeCoerce(new FinalizedF(e, s, i));
        };
    };
};
var mapFinalized = function (dictFunctor) {
    return function (g) {
        return runFinalized(function (e) {
            return function (s) {
                return function (i) {
                    return finalized(function ($18) {
                        return Control_Monad_Free.mapF(Halogen_Query_HalogenF.hoistHalogenF(dictFunctor)(g))(e($18));
                    })(s)(i);
                };
            };
        });
    };
};
var rmapHook = function (dictFunctor) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Finalized) {
                return new Finalized(mapFinalized(dictFunctor)(v)(v1.value0));
            };
            if (v1 instanceof PostRender) {
                return new PostRender(v1.value0);
            };
            throw new Error("Failed pattern match at Halogen.Component.Hook line 65, column 1 - line 66, column 1: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
module.exports = {
    PostRender: PostRender, 
    Finalized: Finalized, 
    rmapHook: rmapHook, 
    lmapHook: lmapHook, 
    mapFinalized: mapFinalized, 
    runFinalized: runFinalized, 
    finalized: finalized
};

},{"../Control.Monad.Free":58,"../Data.NaturalTransformation":160,"../Halogen.Query.HalogenF":194,"../Prelude":203,"../Unsafe.Coerce":206}],176:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Lazy = require("../Data.Lazy");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var runTree = function (k) {
    return function (t) {
        var $5 = Unsafe_Coerce.unsafeCoerce(t);
        return k($5);
    };
};
var mkTree$prime = Unsafe_Coerce.unsafeCoerce;
var thunkTree = runTree(function ($11) {
    return mkTree$prime((function (v) {
        var $6 = {};
        for (var $7 in v) {
            if (v.hasOwnProperty($7)) {
                $6[$7] = v[$7];
            };
        };
        $6.thunk = true;
        return $6;
    })($11));
});
var mkTree = function (dictEq) {
    return function (html) {
        return mkTree$prime({
            slot: Prelude.unit, 
            html: html, 
            eq: Prelude.eq(dictEq), 
            thunk: false
        });
    };
};
var graftTree = function (l) {
    return function (r) {
        return runTree(function (t) {
            return mkTree$prime({
                slot: r(t.slot), 
                html: Prelude["<$>"](Data_Lazy.functorLazy)(Data_Bifunctor.bimap(Halogen_HTML_Core.bifunctorHTML)(graftTree(l)(Prelude.id(Prelude.categoryFn)))(l))(t.html), 
                eq: t.eq, 
                thunk: t.thunk
            });
        });
    };
};
var emptyTree = mkTree$prime({
    slot: Prelude.unit, 
    html: Data_Lazy.defer(function (v) {
        return new Halogen_HTML_Core.Text("");
    }), 
    eq: function (v) {
        return function (v1) {
            return false;
        };
    }, 
    thunk: false
});
module.exports = {
    emptyTree: emptyTree, 
    thunkTree: thunkTree, 
    graftTree: graftTree, 
    runTree: runTree, 
    "mkTree'": mkTree$prime, 
    mkTree: mkTree
};

},{"../Data.Bifunctor":104,"../Data.Lazy":145,"../Data.NaturalTransformation":160,"../Halogen.HTML.Core":180,"../Prelude":203,"../Unsafe.Coerce":206}],177:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Free = require("../Control.Monad.Free");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Control_Monad_ST = require("../Control.Monad.ST");
var Data_Array = require("../Data.Array");
var Data_Array_ST = require("../Data.Array.ST");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor_Coproduct = require("../Data.Functor.Coproduct");
var Data_Lazy = require("../Data.Lazy");
var Data_List = require("../Data.List");
var Data_Map = require("../Data.Map");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_Unsafe = require("../Data.Maybe.Unsafe");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Void = require("../Data.Void");
var Halogen_Component_ChildPath = require("../Halogen.Component.ChildPath");
var Halogen_Component_Hook = require("../Halogen.Component.Hook");
var Halogen_Component_Tree = require("../Halogen.Component.Tree");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_Query = require("../Halogen.Query");
var Halogen_Query_EventSource = require("../Halogen.Query.EventSource");
var Halogen_Query_HalogenF = require("../Halogen.Query.HalogenF");
var Halogen_Query_StateF = require("../Halogen.Query.StateF");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Control_Coroutine_Stalling = require("../Control.Coroutine.Stalling");
var Data_Monoid = require("../Data.Monoid");
var Component = function (x) {
    return x;
};
var SlotConstructor = (function () {
    function SlotConstructor(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SlotConstructor.create = function (value0) {
        return function (value1) {
            return new SlotConstructor(value0, value1);
        };
    };
    return SlotConstructor;
})();
var ChildF = (function () {
    function ChildF(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ChildF.create = function (value0) {
        return function (value1) {
            return new ChildF(value0, value1);
        };
    };
    return ChildF;
})();
var ParentState = function (x) {
    return x;
};
var runChildF = function (v) {
    return v.value1;
};
var renderComponent = function (v) {
    return v.render;
};
var queryComponent = function (v) {
    return v["eval"];
};
var parentState = function (st) {
    return {
        parent: st, 
        children: Data_Map.empty
    };
};
var mapStateFParent = Halogen_Query_StateF.mapState(function (v) {
    return v.parent;
})(function (f) {
    return function (v) {
        return {
            parent: f(v.parent), 
            children: v.children
        };
    };
});
var mergeParentStateF = function ($185) {
    return Control_Monad_Free.liftF(Halogen_Query_HalogenF.StateHF.create(mapStateFParent($185)));
};
var mapStateFChild = function (dictOrd) {
    return function (p) {
        return Halogen_Query_StateF.mapState(function (v) {
            return Data_Maybe_Unsafe.fromJust(Prelude["<$>"](Data_Maybe.functorMaybe)(function (v1) {
                return v1.state;
            })(Data_Map.lookup(dictOrd)(p)(v.children)));
        })(function (f) {
            return function (v) {
                return {
                    parent: v.parent, 
                    children: Data_Map.update(dictOrd)(function (child) {
                        return new Data_Maybe.Just({
                            component: child.component, 
                            state: f(child.state), 
                            memo: Data_Maybe.Nothing.value
                        });
                    })(p)(v.children)
                };
            };
        });
    };
};
var liftQuery = Halogen_Query.liftH;
var liftChildF = function (dictFunctor) {
    return Control_Monad_Free.mapF(Halogen_Query_HalogenF.transformHF(dictFunctor)(Prelude.id(Prelude.categoryFn))(Data_Functor_Coproduct.right)(Prelude.id(Prelude.categoryFn)));
};
var queryParent = function (dictFunctor) {
    return function (f) {
        return function ($186) {
            return Control_Monad_Free.foldFree(Control_Monad_Free.freeMonadRec)(function (h) {
                if (h instanceof Halogen_Query_HalogenF.StateHF) {
                    return mergeParentStateF(h.value0);
                };
                if (h instanceof Halogen_Query_HalogenF.SubscribeHF) {
                    return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.SubscribeHF(Control_Monad_Free_Trans.interpret(Control_Coroutine_Stalling.functorStallF)(dictFunctor)(Data_Bifunctor.lmap(Control_Coroutine_Stalling.bifunctorStallF)(Data_Functor_Coproduct.left))(Halogen_Query_EventSource.runEventSource(Halogen_Query_EventSource.fromParentEventSource(h.value0))), h.value1));
                };
                if (h instanceof Halogen_Query_HalogenF.QueryHF) {
                    return liftChildF(dictFunctor)(h.value0);
                };
                if (h instanceof Halogen_Query_HalogenF.RenderHF) {
                    return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.RenderHF(h.value0, h.value1));
                };
                if (h instanceof Halogen_Query_HalogenF.RenderPendingHF) {
                    return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.RenderPendingHF(h.value0));
                };
                if (h instanceof Halogen_Query_HalogenF.HaltHF) {
                    return Control_Monad_Free.liftF(Halogen_Query_HalogenF.HaltHF.value);
                };
                throw new Error("Failed pattern match at Halogen.Component line 489, column 5 - line 498, column 1: " + [ h.constructor.name ]);
            })(f($186));
        };
    };
};
var lifecycleComponent = function (spec) {
    var renderTree = function (html) {
        return Halogen_Component_Tree["mkTree'"]({
            slot: Prelude.unit, 
            html: Data_Lazy.defer(function (v) {
                return Unsafe_Coerce.unsafeCoerce(html);
            }), 
            eq: function (v) {
                return function (v1) {
                    return false;
                };
            }, 
            thunk: false
        });
    };
    return {
        render: function (s) {
            return {
                state: s, 
                hooks: [  ], 
                tree: renderTree(spec.render(s))
            };
        }, 
        "eval": spec["eval"], 
        initializer: spec.initializer, 
        finalizers: function (s) {
            return Data_Maybe.maybe([  ])(function (i) {
                return [ Halogen_Component_Hook.finalized(spec["eval"])(s)(i) ];
            })(spec.finalizer);
        }
    };
};
var interpret = function (dictFunctor) {
    return function (nat) {
        return function (v) {
            var render$prime = function (st) {
                var $100 = v.render(st);
                return {
                    state: $100.state, 
                    hooks: Prelude["<$>"](Prelude.functorArray)(Halogen_Component_Hook.rmapHook(dictFunctor)(nat))($100.hooks), 
                    tree: $100.tree
                };
            };
            return {
                render: render$prime, 
                "eval": function ($187) {
                    return Control_Monad_Free.mapF(Halogen_Query_HalogenF.hoistHalogenF(dictFunctor)(nat))(v["eval"]($187));
                }, 
                initializer: v.initializer, 
                finalizers: Prelude["<$>"](Prelude.functorFn)(Prelude.map(Prelude.functorArray)(Halogen_Component_Hook.mapFinalized(dictFunctor)(nat)))(v.finalizers)
            };
        };
    };
};
var initializeComponent = function (v) {
    return v.initializer;
};
var functorChildF = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        return function (v) {
            return new ChildF(v.value0, Prelude["<$>"](dictFunctor)(f)(v.value1));
        };
    });
};
var finalizeComponent = function (v) {
    return v.finalizers;
};
var parentFinalizers = function ($$eval) {
    return function (fin) {
        return function (v) {
            var parentFin = function (i) {
                return [ Halogen_Component_Hook.finalized($$eval)(parentState(v.parent))(Data_Functor_Coproduct.left(i)) ];
            };
            var childFin = function (child) {
                return finalizeComponent(child.component)(child.state);
            };
            return Prelude["<>"](Prelude.semigroupArray)(Data_Foldable.foldMap(Data_Map.foldableMap)(Data_Monoid.monoidArray)(childFin)(v.children))(Data_Foldable.foldMap(Data_Foldable.foldableMaybe)(Data_Monoid.monoidArray)(parentFin)(fin));
        };
    };
};
var renderParent = function (dictOrd) {
    return function (render) {
        return function (v) {
            var installChild = function (v1) {
                return function (v2) {
                    var update = function (hs) {
                        return function (c) {
                            return {
                                children: Data_Map.insert(dictOrd)(v1.value0)(c)(v2.children), 
                                removed: Data_Map["delete"](dictOrd)(v1.value0)(v2.removed), 
                                hooks: Prelude["<>"](Prelude.semigroupArray)(v2.hooks)(hs)
                            };
                        };
                    };
                    var renderChild = function (v3) {
                        return function (v4) {
                            if (v4.memo instanceof Data_Maybe.Just) {
                                return new Data_Tuple.Tuple(Halogen_HTML_Core.Slot.create(Halogen_Component_Tree.thunkTree(v4.memo.value0)), update([  ])(v4));
                            };
                            var r = renderComponent(v4.component)(v4.state);
                            var adapt = function (a) {
                                return Data_Functor_Coproduct.right(new ChildF(v1.value0, a));
                            };
                            var hooks$prime = Prelude["<$>"](Prelude.functorArray)(Halogen_Component_Hook.lmapHook(adapt))(Data_Maybe.maybe(r.hooks)(Prelude.flip(Data_Array.cons)(r.hooks))(v3));
                            var tree = Halogen_Component_Tree.graftTree(adapt)(Prelude["const"](v1.value0))(r.tree);
                            return Data_Tuple.Tuple.create(new Halogen_HTML_Core.Slot(tree))(update(hooks$prime)({
                                component: v4.component, 
                                state: r.state, 
                                memo: new Data_Maybe.Just(tree)
                            }));
                        };
                    };
                    var $121 = Data_Map.lookup(dictOrd)(v1.value0)(v.children);
                    if ($121 instanceof Data_Maybe.Just) {
                        return renderChild(Data_Maybe.Nothing.value)($121.value0);
                    };
                    if ($121 instanceof Data_Maybe.Nothing) {
                        var def$prime = v1.value1(Prelude.unit);
                        var hook = Prelude["<$>"](Data_Maybe.functorMaybe)(Halogen_Component_Hook.PostRender.create)(initializeComponent(def$prime.component));
                        return renderChild(hook)({
                            component: def$prime.component, 
                            state: def$prime.initialState, 
                            memo: Data_Maybe.Nothing.value
                        });
                    };
                    throw new Error("Failed pattern match at Halogen.Component line 444, column 5 - line 457, column 5: " + [ $121.constructor.name ]);
                };
            };
            var install = function (v1) {
                return function (st) {
                    if (v1 instanceof Halogen_HTML_Core.Text) {
                        return new Data_Tuple.Tuple(new Halogen_HTML_Core.Text(v1.value0), st);
                    };
                    if (v1 instanceof Halogen_HTML_Core.Slot) {
                        return installChild(v1.value0)(st);
                    };
                    if (v1 instanceof Halogen_HTML_Core.Element) {
                        return Control_Monad_Eff.runPure(function __do() {
                            var v2 = Data_Array_ST.emptySTArray();
                            var v3 = {
                                value: st
                            };
                            Control_Monad_Eff.foreachE(v1.value3)(function (el) {
                                return function __do() {
                                    var $135 = install(el)(v3.value);
                                    Prelude["void"](Control_Monad_Eff.functorEff)(Data_Array_ST.pushSTArray(v2)($135.value0))();
                                    return Prelude["void"](Control_Monad_Eff.functorEff)(Control_Monad_ST.writeSTRef(v3)($135.value1))();
                                };
                            })();
                            return new Data_Tuple.Tuple(Halogen_HTML_Core.Element.create(v1.value0)(v1.value1)(Prelude["<$>"](Prelude.functorArray)(Prelude.map(Halogen_HTML_Core.functorProp)(Data_Functor_Coproduct.left))(v1.value2))(Unsafe_Coerce.unsafeCoerce(v2)), v3.value);
                        });
                    };
                    throw new Error("Failed pattern match at Halogen.Component line 406, column 1 - line 481, column 1: " + [ v1.constructor.name, st.constructor.name ]);
                };
            };
            var init = {
                children: Data_Map.empty, 
                removed: v.children, 
                hooks: [  ]
            };
            var finalizeChild = function (child) {
                return Prelude.map(Prelude.functorArray)(Halogen_Component_Hook.Finalized.create)(finalizeComponent(child.component)(child.state));
            };
            var $143 = install(render(v.parent))(init);
            return {
                state: {
                    parent: v.parent, 
                    children: $143.value1.children
                }, 
                hooks: Prelude["<>"](Prelude.semigroupArray)(Data_Foldable.foldMap(Data_Map.foldableMap)(Data_Monoid.monoidArray)(finalizeChild)($143.value1.removed))($143.value1.hooks), 
                tree: Halogen_Component_Tree.mkTree(dictOrd["__superclass_Prelude.Eq_0"]())(Data_Lazy.defer(function (v1) {
                    return $143.value0;
                }))
            };
        };
    };
};
var emptyResult = function (state) {
    return {
        state: state, 
        hooks: [  ], 
        tree: Halogen_Component_Tree.emptyTree
    };
};
var transform = function (dictFunctor) {
    return function (reviewS) {
        return function (previewS) {
            return function (reviewQ) {
                return function (previewQ) {
                    return function (v) {
                        var render$prime = function (st) {
                            var $152 = v.render(st);
                            return {
                                state: reviewS($152.state), 
                                hooks: Prelude["<$>"](Prelude.functorArray)(Halogen_Component_Hook.lmapHook(reviewQ))($152.hooks), 
                                tree: Halogen_Component_Tree.graftTree(reviewQ)(Prelude.id(Prelude.categoryFn))($152.tree)
                            };
                        };
                        var modifyState = function (f) {
                            return function (s$prime) {
                                return Data_Maybe.maybe(s$prime)(function ($188) {
                                    return reviewS(f($188));
                                })(previewS(s$prime));
                            };
                        };
                        var go = function (v1) {
                            if (v1 instanceof Halogen_Query_HalogenF.StateHF && v1.value0 instanceof Halogen_Query_StateF.Get) {
                                return Control_Bind["=<<"](Control_Monad_Free.freeBind)(function ($189) {
                                    return Control_Monad_Free.liftF(Data_Maybe.maybe(Halogen_Query_HalogenF.HaltHF.value)(function (st$prime) {
                                        return new Halogen_Query_HalogenF.StateHF(new Halogen_Query_StateF.Get(function ($190) {
                                            return v1.value0.value0(Prelude["const"](st$prime)($190));
                                        }));
                                    })(previewS($189)));
                                })(Halogen_Query.get);
                            };
                            if (v1 instanceof Halogen_Query_HalogenF.StateHF && v1.value0 instanceof Halogen_Query_StateF.Modify) {
                                return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.StateHF(new Halogen_Query_StateF.Modify(modifyState(v1.value0.value0), v1.value0.value1)));
                            };
                            if (v1 instanceof Halogen_Query_HalogenF.SubscribeHF) {
                                return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.SubscribeHF(Control_Monad_Free_Trans.interpret(Control_Coroutine_Stalling.functorStallF)(dictFunctor)(Data_Bifunctor.lmap(Control_Coroutine_Stalling.bifunctorStallF)(reviewQ))(Halogen_Query_EventSource.runEventSource(v1.value0)), v1.value1));
                            };
                            if (v1 instanceof Halogen_Query_HalogenF.QueryHF) {
                                return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.QueryHF(v1.value0));
                            };
                            if (v1 instanceof Halogen_Query_HalogenF.RenderHF) {
                                return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.RenderHF(v1.value0, v1.value1));
                            };
                            if (v1 instanceof Halogen_Query_HalogenF.RenderPendingHF) {
                                return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.RenderPendingHF(v1.value0));
                            };
                            if (v1 instanceof Halogen_Query_HalogenF.HaltHF) {
                                return Control_Monad_Free.liftF(Halogen_Query_HalogenF.HaltHF.value);
                            };
                            throw new Error("Failed pattern match at Halogen.Component line 554, column 3 - line 556, column 3: " + [ v1.constructor.name ]);
                        };
                        return {
                            render: function (st) {
                                return Data_Maybe.maybe(emptyResult(st))(render$prime)(previewS(st));
                            }, 
                            "eval": function ($191) {
                                return Data_Maybe.maybe(Control_Monad_Free.liftF(Halogen_Query_HalogenF.HaltHF.value))(function ($192) {
                                    return Control_Monad_Free.foldFree(Control_Monad_Free.freeMonadRec)(go)(v["eval"]($192));
                                })(previewQ($191));
                            }, 
                            initializer: Prelude["<$>"](Data_Maybe.functorMaybe)(reviewQ)(v.initializer), 
                            finalizers: function ($193) {
                                return Data_Maybe.maybe([  ])(v.finalizers)(previewS($193));
                            }
                        };
                    };
                };
            };
        };
    };
};
var transformChild = function (dictFunctor) {
    return function (i) {
        return transform(dictFunctor)(Halogen_Component_ChildPath.injState(i))(Halogen_Component_ChildPath.prjState(i))(Halogen_Component_ChildPath.injQuery(i))(Halogen_Component_ChildPath.prjQuery(i));
    };
};
var component = function (spec) {
    return lifecycleComponent({
        render: spec.render, 
        "eval": spec["eval"], 
        initializer: Data_Maybe.Nothing.value, 
        finalizer: Data_Maybe.Nothing.value
    });
};
var childSlots = function (dictFunctor) {
    return function (dictOrd) {
        return Prelude.bind(Control_Monad_Free.freeBind)(Halogen_Query.get)(function (v) {
            return Prelude.pure(Control_Monad_Free.freeApplicative)(Data_Map.keys(v.children));
        });
    };
};
var bracketQuery = function (f) {
    return Prelude.bind(Control_Monad_Free.freeBind)(Control_Monad_Free.liftF(new Halogen_Query_HalogenF.RenderPendingHF(Prelude.id(Prelude.categoryFn))))(function (v) {
        return Prelude.bind(Control_Monad_Free.freeBind)((function () {
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Halogen_Query_HalogenF.Pending) {
                return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.RenderHF(Data_Maybe.Nothing.value, Prelude.unit));
            };
            return Prelude.pure(Control_Monad_Free.freeApplicative)(Prelude.unit);
        })())(function () {
            return Prelude.bind(Control_Monad_Free.freeBind)(f)(function (v1) {
                return Prelude.bind(Control_Monad_Free.freeBind)(Control_Monad_Free.liftF(new Halogen_Query_HalogenF.RenderPendingHF(Prelude.id(Prelude.categoryFn))))(function (v2) {
                    return Prelude.bind(Control_Monad_Free.freeBind)(Data_Foldable.for_(Control_Monad_Free.freeApplicative)(Data_Foldable.foldableMaybe)(v)(function (v3) {
                        return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.RenderHF(new Data_Maybe.Just(Halogen_Query_HalogenF.Deferred.value), Prelude.unit));
                    }))(function () {
                        return Prelude.pure(Control_Monad_Free.freeApplicative)(v1);
                    });
                });
            });
        });
    });
};
var mkQueries$prime = function (dictFunctor) {
    return function (dictOrd) {
        return function (dictOrd1) {
            return function (i) {
                return function (q) {
                    var mkChildQuery = function (v) {
                        return Data_Traversable["for"](Control_Monad_Free.freeApplicative)(Data_Traversable.traversableMaybe)(Halogen_Component_ChildPath.prjSlot(i)(v.value0))(function (p) {
                            return Prelude["<$>"](Control_Monad_Free.freeFunctor)(Data_Tuple.Tuple.create(p))(Control_Monad_Free.mapF(Halogen_Query_HalogenF.transformHF(dictFunctor)(mapStateFChild(dictOrd)(v.value0))(ChildF.create(v.value0))(Prelude.id(Prelude.categoryFn)))(queryComponent(v.value1.component)(Halogen_Component_ChildPath.injQuery(i)(q))));
                        });
                    };
                    return bracketQuery(Prelude.bind(Control_Monad_Free.freeBind)(Halogen_Query.get)(function (v) {
                        return Prelude["<$>"](Control_Monad_Free.freeFunctor)(function ($194) {
                            return Data_Map.fromList(dictOrd1)(Data_List.catMaybes($194));
                        })(Data_Traversable.traverse(Data_List.traversableList)(Control_Monad_Free.freeApplicative)(mkChildQuery)(Data_Map.toList(v.children)));
                    }));
                };
            };
        };
    };
};
var mkQueries = function (dictFunctor) {
    return function (dictOrd) {
        return mkQueries$prime(dictFunctor)(dictOrd)(dictOrd)(new Halogen_Component_ChildPath.ChildPath(function (dictChoice) {
            return function (dictApplicative) {
                return Prelude.id(Prelude.categoryFn);
            };
        }, function (dictChoice) {
            return function (dictApplicative) {
                return Prelude.id(Prelude.categoryFn);
            };
        }, function (dictChoice) {
            return function (dictApplicative) {
                return Prelude.id(Prelude.categoryFn);
            };
        }));
    };
};
var queryAll = function (dictFunctor) {
    return function (dictOrd) {
        return function (q) {
            return liftQuery(mkQueries(dictFunctor)(dictOrd)(q));
        };
    };
};
var queryAll$prime = function (dictFunctor) {
    return function (dictOrd) {
        return function (dictOrd1) {
            return function (i) {
                return function (q) {
                    return liftQuery(mkQueries$prime(dictFunctor)(dictOrd1)(dictOrd)(i)(q));
                };
            };
        };
    };
};
var mkQuery = function (dictFunctor) {
    return function (dictOrd) {
        return function (p) {
            return function (q) {
                return bracketQuery(Prelude.bind(Control_Monad_Free.freeBind)(Halogen_Query.get)(function (v) {
                    return Data_Traversable["for"](Control_Monad_Free.freeApplicative)(Data_Traversable.traversableMaybe)(Data_Map.lookup(dictOrd)(p)(v.children))(function (child) {
                        return Control_Monad_Free.mapF(Halogen_Query_HalogenF.transformHF(dictFunctor)(mapStateFChild(dictOrd)(p))(ChildF.create(p))(Prelude.id(Prelude.categoryFn)))(queryComponent(child.component)(q));
                    });
                }));
            };
        };
    };
};
var mkQuery$prime = function (dictFunctor) {
    return function (dictOrd) {
        return function (i) {
            return function (p) {
                return function (q) {
                    return mkQuery(dictFunctor)(dictOrd)(Halogen_Component_ChildPath.injSlot(i)(p))(Halogen_Component_ChildPath.injQuery(i)(q));
                };
            };
        };
    };
};
var query$prime = function (dictFunctor) {
    return function (dictOrd) {
        return function (i) {
            return function (p) {
                return function (q) {
                    return liftQuery(mkQuery$prime(dictFunctor)(dictOrd)(i)(p)(q));
                };
            };
        };
    };
};
var query = function (dictFunctor) {
    return function (dictOrd) {
        return function (p) {
            return function (q) {
                return liftQuery(mkQuery(dictFunctor)(dictOrd)(p)(q));
            };
        };
    };
};
var queryChild = function (dictFunctor) {
    return function (dictOrd) {
        return function (v) {
            return Prelude[">>="](Control_Monad_Free.freeBind)(Control_Monad_Free.mapF(Halogen_Query_HalogenF.transformHF(dictFunctor)(Prelude.id(Prelude.categoryFn))(Data_Functor_Coproduct.right)(Prelude.id(Prelude.categoryFn)))(mkQuery(dictFunctor)(dictOrd)(v.value0)(v.value1)))(Data_Maybe.maybe(Control_Monad_Free.liftF(Halogen_Query_HalogenF.HaltHF.value))(Prelude.pure(Control_Monad_Free.freeApplicative)));
        };
    };
};
var lifecycleParentComponent = function (dictFunctor) {
    return function (dictOrd) {
        return function (spec) {
            var $$eval = Data_Functor_Coproduct.coproduct(queryParent(dictFunctor)(spec["eval"]))((function () {
                if (spec.peek instanceof Data_Maybe.Nothing) {
                    return queryChild(dictFunctor)(dictOrd);
                };
                if (spec.peek instanceof Data_Maybe.Just) {
                    return function (q) {
                        return Control_Apply["<*"](Control_Monad_Free.freeApply)(queryChild(dictFunctor)(dictOrd)(q))(queryParent(dictFunctor)(spec.peek.value0)(q));
                    };
                };
                throw new Error("Failed pattern match at Halogen.Component line 208, column 10 - line 212, column 1: " + [ spec.peek.constructor.name ]);
            })());
            return {
                render: renderParent(dictOrd)(spec.render), 
                "eval": $$eval, 
                initializer: Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Functor_Coproduct.left)(spec.initializer), 
                finalizers: parentFinalizers($$eval)(spec.finalizer)
            };
        };
    };
};
var parentComponent = function (dictFunctor) {
    return function (dictOrd) {
        return function (spec) {
            return lifecycleParentComponent(dictFunctor)(dictOrd)({
                render: spec.render, 
                "eval": spec["eval"], 
                peek: spec.peek, 
                initializer: Data_Maybe.Nothing.value, 
                finalizer: Data_Maybe.Nothing.value
            });
        };
    };
};
module.exports = {
    ChildF: ChildF, 
    SlotConstructor: SlotConstructor, 
    finalizeComponent: finalizeComponent, 
    initializeComponent: initializeComponent, 
    queryComponent: queryComponent, 
    renderComponent: renderComponent, 
    interpret: interpret, 
    transformChild: transformChild, 
    transform: transform, 
    childSlots: childSlots, 
    "queryAll'": queryAll$prime, 
    queryAll: queryAll, 
    "query'": query$prime, 
    query: query, 
    liftQuery: liftQuery, 
    "mkQueries'": mkQueries$prime, 
    mkQueries: mkQueries, 
    "mkQuery'": mkQuery$prime, 
    mkQuery: mkQuery, 
    runChildF: runChildF, 
    parentState: parentState, 
    lifecycleParentComponent: lifecycleParentComponent, 
    parentComponent: parentComponent, 
    lifecycleComponent: lifecycleComponent, 
    component: component, 
    functorChildF: functorChildF
};

},{"../Control.Apply":28,"../Control.Bind":31,"../Control.Coroutine.Stalling":34,"../Control.Monad.Eff":54,"../Control.Monad.Free":58,"../Control.Monad.Free.Trans":57,"../Control.Monad.ST":67,"../Data.Array":102,"../Data.Array.ST":100,"../Data.Bifunctor":104,"../Data.Foldable":120,"../Data.Functor.Coproduct":132,"../Data.Lazy":145,"../Data.List":146,"../Data.Map":147,"../Data.Maybe":152,"../Data.Maybe.Unsafe":151,"../Data.Monoid":159,"../Data.NaturalTransformation":160,"../Data.Traversable":170,"../Data.Tuple":171,"../Data.Void":173,"../Halogen.Component.ChildPath":174,"../Halogen.Component.Hook":175,"../Halogen.Component.Tree":176,"../Halogen.HTML.Core":180,"../Halogen.Query":196,"../Halogen.Query.EventSource":193,"../Halogen.Query.HalogenF":194,"../Halogen.Query.StateF":195,"../Prelude":203,"../Unsafe.Coerce":206}],178:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Bind = require("../Control.Bind");
var Control_Coroutine = require("../Control.Coroutine");
var Control_Coroutine_Stalling_1 = require("../Control.Coroutine.Stalling");
var Control_Coroutine_Stalling_1 = require("../Control.Coroutine.Stalling");
var Control_Monad = require("../Control.Monad");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_AVar = require("../Control.Monad.Aff.AVar");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Free = require("../Control.Monad.Free");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_State = require("../Control.Monad.State");
var Control_Monad_Trans = require("../Control.Monad.Trans");
var Control_Plus = require("../Control.Plus");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_List = require("../Data.List");
var Data_Maybe = require("../Data.Maybe");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var Data_Tuple = require("../Data.Tuple");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_Node_Node = require("../DOM.Node.Node");
var Halogen_Component = require("../Halogen.Component");
var Halogen_Component_Hook = require("../Halogen.Component.Hook");
var Halogen_Effects = require("../Halogen.Effects");
var Halogen_HTML_Renderer_VirtualDOM = require("../Halogen.HTML.Renderer.VirtualDOM");
var Halogen_Internal_VirtualDOM = require("../Halogen.Internal.VirtualDOM");
var Halogen_Query = require("../Halogen.Query");
var Halogen_Query_HalogenF = require("../Halogen.Query.HalogenF");
var Halogen_Query_EventSource = require("../Halogen.Query.EventSource");
var Halogen_Query_StateF = require("../Halogen.Query.StateF");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Data_Identity = require("../Data.Identity");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var onInitializers = function (dictFoldable) {
    return function (f) {
        var go = function (v) {
            return function (as) {
                if (v instanceof Halogen_Component_Hook.PostRender) {
                    return Data_List[":"](f(v.value0))(as);
                };
                return as;
            };
        };
        return Data_Foldable.foldr(dictFoldable)(go)(Data_List.Nil.value);
    };
};
var onFinalizers = function (dictFoldable) {
    return function (f) {
        var go = function (v) {
            return function (as) {
                if (v instanceof Halogen_Component_Hook.Finalized) {
                    return Data_List[":"](f(v.value0))(as);
                };
                return as;
            };
        };
        return Data_Foldable.foldr(dictFoldable)(go)(Data_List.Nil.value);
    };
};
var runUI = function (c) {
    return function (s) {
        return function (element) {
            var driver$prime = function (e) {
                return function (s1) {
                    return function (i) {
                        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar["makeVar'"](s1))(function (v) {
                            return Prelude.flip(Control_Monad_Free.runFreeM(Halogen_Query_HalogenF.functorHalogenF(Control_Monad_Aff.functorAff))(Control_Monad_Aff.monadRecAff))(e(i))(function (h) {
                                if (h instanceof Halogen_Query_HalogenF.StateHF) {
                                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(v))(function (v1) {
                                        var $29 = Control_Monad_State.runState(Halogen_Query_StateF.stateN(Control_Monad_State_Trans.monadStateT(Data_Identity.monadIdentity))(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(h.value0))(v1);
                                        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(v)($29.value1))(function () {
                                            return Prelude.pure(Control_Monad_Aff.applicativeAff)($29.value0);
                                        });
                                    });
                                };
                                if (h instanceof Halogen_Query_HalogenF.SubscribeHF) {
                                    return Prelude.pure(Control_Monad_Aff.applicativeAff)(h.value1);
                                };
                                if (h instanceof Halogen_Query_HalogenF.RenderHF) {
                                    return Prelude.pure(Control_Monad_Aff.applicativeAff)(h.value1);
                                };
                                if (h instanceof Halogen_Query_HalogenF.RenderPendingHF) {
                                    return Prelude.pure(Control_Monad_Aff.applicativeAff)(h.value0(Data_Maybe.Nothing.value));
                                };
                                if (h instanceof Halogen_Query_HalogenF.QueryHF) {
                                    return h.value0;
                                };
                                if (h instanceof Halogen_Query_HalogenF.HaltHF) {
                                    return Control_Plus.empty(Control_Monad_Aff.plusAff);
                                };
                                throw new Error("Failed pattern match at Halogen.Driver line 146, column 7 - line 159, column 3: " + [ h.constructor.name ]);
                            });
                        });
                    };
                };
            };
            var render = function (ref) {
                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(ref))(function (v) {
                    if (v.renderPaused) {
                        return Control_Monad_Aff_AVar.putVar(ref)((function () {
                            var $41 = {};
                            for (var $42 in v) {
                                if (v.hasOwnProperty($42)) {
                                    $41[$42] = v[$42];
                                };
                            };
                            $41.renderPending = true;
                            return $41;
                        })());
                    };
                    if (!v.renderPaused) {
                        var rc = Halogen_Component.renderComponent(c)(v.state);
                        var vtree$prime = Halogen_HTML_Renderer_VirtualDOM.renderTree(driver(ref))(rc.tree);
                        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Halogen_Internal_VirtualDOM.patch(Halogen_Internal_VirtualDOM.diff(v.vtree)(vtree$prime))(v.node)))(function (v1) {
                            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(ref)({
                                node: v1, 
                                vtree: vtree$prime, 
                                state: rc.state, 
                                renderPending: false, 
                                renderPaused: true
                            }))(function () {
                                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAll(Data_List.foldableList)(onFinalizers(Data_Foldable.foldableArray)(Halogen_Component_Hook.runFinalized(driver$prime))(rc.hooks)))(function () {
                                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAll(Data_List.foldableList)(onInitializers(Data_Foldable.foldableArray)(driver(ref))(rc.hooks)))(function () {
                                        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.modifyVar(function (v2) {
                                            var $44 = {};
                                            for (var $45 in v2) {
                                                if (v2.hasOwnProperty($45)) {
                                                    $44[$45] = v2[$45];
                                                };
                                            };
                                            $44.renderPaused = false;
                                            return $44;
                                        })(ref))(function () {
                                            return flushRender(ref);
                                        });
                                    });
                                });
                            });
                        });
                    };
                    throw new Error("Failed pattern match at Halogen.Driver line 162, column 5 - line 180, column 3: " + [ v.renderPaused.constructor.name ]);
                });
            };
            var flushRender = Control_Monad_Rec_Class.tailRecM(Control_Monad_Aff.monadRecAff)(function (ref) {
                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(ref))(function (v) {
                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(ref)(v))(function () {
                        var $47 = !v.renderPending;
                        if ($47) {
                            return Prelude.pure(Control_Monad_Aff.applicativeAff)(new Data_Either.Right(Prelude.unit));
                        };
                        if (!$47) {
                            return Prelude.bind(Control_Monad_Aff.bindAff)(render(ref))(function () {
                                return Prelude.pure(Control_Monad_Aff.applicativeAff)(new Data_Either.Left(ref));
                            });
                        };
                        throw new Error("Failed pattern match at Halogen.Driver line 184, column 5 - line 190, column 1: " + [ $47.constructor.name ]);
                    });
                });
            });
            var $$eval = function (ref) {
                return function (rpRef) {
                    return function (h) {
                        if (h instanceof Halogen_Query_HalogenF.StateHF) {
                            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(ref))(function (v) {
                                if (h.value0 instanceof Halogen_Query_StateF.Get) {
                                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(ref)(v))(function () {
                                        return Prelude.pure(Control_Monad_Aff.applicativeAff)(h.value0.value0(v.state));
                                    });
                                };
                                if (h.value0 instanceof Halogen_Query_StateF.Modify) {
                                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(rpRef))(function (v1) {
                                        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(ref)((function () {
                                            var $53 = {};
                                            for (var $54 in v) {
                                                if (v.hasOwnProperty($54)) {
                                                    $53[$54] = v[$54];
                                                };
                                            };
                                            $53.state = h.value0.value0(v.state);
                                            return $53;
                                        })()))(function () {
                                            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(rpRef)(new Data_Maybe.Just(Halogen_Query_HalogenF.Pending.value)))(function () {
                                                return Prelude.pure(Control_Monad_Aff.applicativeAff)(h.value0.value1);
                                            });
                                        });
                                    });
                                };
                                throw new Error("Failed pattern match at Halogen.Driver line 108, column 9 - line 117, column 7: " + [ h.value0.constructor.name ]);
                            });
                        };
                        if (h instanceof Halogen_Query_HalogenF.SubscribeHF) {
                            var producer = Halogen_Query_EventSource.runEventSource(h.value0);
                            var consumer = Control_Monad_Rec_Class.forever(Control_Monad_Free_Trans.monadRecFreeT(Control_Coroutine.functorAwait)(Control_Monad_Aff.monadAff))(Control_Bind["=<<"](Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Control_Monad_Aff.monadAff))(function ($72) {
                                return Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))(Control_Monad_Aff.monadAff)(driver(ref)($72));
                            })(Control_Coroutine["await"](Control_Monad_Aff.monadAff)));
                            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAff(Control_Coroutine_Stalling_1.runStallingProcess(Control_Monad_Aff.monadRecAff)(Control_Coroutine_Stalling_1["$$?"](Control_Monad_Aff.monadRecAff)(producer)(consumer))))(function () {
                                return Prelude.pure(Control_Monad_Aff.applicativeAff)(h.value1);
                            });
                        };
                        if (h instanceof Halogen_Query_HalogenF.RenderHF) {
                            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.modifyVar(Prelude["const"](h.value0))(rpRef))(function () {
                                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad.when(Control_Monad_Aff.monadAff)(Data_Maybe.isNothing(h.value0))(render(ref)))(function () {
                                    return Prelude.pure(Control_Monad_Aff.applicativeAff)(h.value1);
                                });
                            });
                        };
                        if (h instanceof Halogen_Query_HalogenF.RenderPendingHF) {
                            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(rpRef))(function (v) {
                                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(rpRef)(v))(function () {
                                    return Prelude.pure(Control_Monad_Aff.applicativeAff)(h.value0(v));
                                });
                            });
                        };
                        if (h instanceof Halogen_Query_HalogenF.QueryHF) {
                            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(rpRef))(function (v) {
                                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad.when(Control_Monad_Aff.monadAff)(Data_Maybe.isJust(v))(render(ref)))(function () {
                                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(rpRef)(Data_Maybe.Nothing.value))(function () {
                                        return h.value0;
                                    });
                                });
                            });
                        };
                        if (h instanceof Halogen_Query_HalogenF.HaltHF) {
                            return Control_Plus.empty(Control_Monad_Aff.plusAff);
                        };
                        throw new Error("Failed pattern match at Halogen.Driver line 105, column 5 - line 137, column 3: " + [ h.constructor.name ]);
                    };
                };
            };
            var driver = function (ref) {
                return function (q) {
                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar["makeVar'"](Data_Maybe.Nothing.value))(function (v) {
                        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Free.runFreeM(Halogen_Query_HalogenF.functorHalogenF(Control_Monad_Aff.functorAff))(Control_Monad_Aff.monadRecAff)($$eval(ref)(v))(Halogen_Component.queryComponent(c)(q)))(function (v1) {
                            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(v))(function (v2) {
                                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad.when(Control_Monad_Aff.monadAff)(Data_Maybe.isJust(v2))(render(ref)))(function () {
                                    return Prelude.pure(Control_Monad_Aff.applicativeAff)(v1);
                                });
                            });
                        });
                    });
                };
            };
            return Prelude["<$>"](Control_Monad_Aff.functorAff)(function (v) {
                return v.driver;
            })(Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.makeVar)(function (v) {
                var rc = Halogen_Component.renderComponent(c)(s);
                var dr = driver(v);
                var vtree = Halogen_HTML_Renderer_VirtualDOM.renderTree(dr)(rc.tree);
                var node = Halogen_Internal_VirtualDOM.createElement(vtree);
                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(v)({
                    node: node, 
                    vtree: vtree, 
                    state: rc.state, 
                    renderPending: false, 
                    renderPaused: true
                }))(function () {
                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(DOM_Node_Node.appendChild(DOM_HTML_Types.htmlElementToNode(node))(DOM_HTML_Types.htmlElementToNode(element))))(function () {
                        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAll(Data_List.foldableList)(onInitializers(Data_Foldable.foldableArray)(dr)(rc.hooks)))(function () {
                            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAff(Data_Maybe.maybe(Prelude.pure(Control_Monad_Aff.applicativeAff)(Prelude.unit))(dr)(Halogen_Component.initializeComponent(c))))(function () {
                                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.modifyVar(function (v1) {
                                    var $70 = {};
                                    for (var $71 in v1) {
                                        if (v1.hasOwnProperty($71)) {
                                            $70[$71] = v1[$71];
                                        };
                                    };
                                    $70.renderPaused = false;
                                    return $70;
                                })(v))(function () {
                                    return Prelude.bind(Control_Monad_Aff.bindAff)(flushRender(v))(function () {
                                        return Prelude.pure(Control_Monad_Aff.applicativeAff)({
                                            driver: dr
                                        });
                                    });
                                });
                            });
                        });
                    });
                });
            }));
        };
    };
};
module.exports = {
    runUI: runUI
};

},{"../Control.Bind":31,"../Control.Coroutine":35,"../Control.Coroutine.Stalling":34,"../Control.Monad":75,"../Control.Monad.Aff":43,"../Control.Monad.Aff.AVar":39,"../Control.Monad.Eff.Class":46,"../Control.Monad.Free":58,"../Control.Monad.Free.Trans":57,"../Control.Monad.Rec.Class":65,"../Control.Monad.State":70,"../Control.Monad.State.Trans":69,"../Control.Monad.Trans":71,"../Control.Plus":77,"../DOM.HTML.Types":87,"../DOM.Node.Node":93,"../Data.Either":114,"../Data.Foldable":120,"../Data.Identity":137,"../Data.List":146,"../Data.Maybe":152,"../Data.NaturalTransformation":160,"../Data.Tuple":171,"../Halogen.Component":177,"../Halogen.Component.Hook":175,"../Halogen.Effects":179,"../Halogen.HTML.Renderer.VirtualDOM":189,"../Halogen.Internal.VirtualDOM":192,"../Halogen.Query":196,"../Halogen.Query.EventSource":193,"../Halogen.Query.HalogenF":194,"../Halogen.Query.StateF":195,"../Prelude":203}],179:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Control_Monad_Aff_AVar = require("../Control.Monad.Aff.AVar");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var DOM = require("../DOM");
module.exports = {};

},{"../Control.Monad.Aff.AVar":39,"../Control.Monad.Eff.Exception":48,"../DOM":98}],180:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Exists = require("../Data.Exists");
var Data_ExistsR = require("../Data.ExistsR");
var Data_Maybe = require("../Data.Maybe");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var Halogen_HTML_Events_Handler = require("../Halogen.HTML.Events.Handler");
var Halogen_HTML_Events_Types = require("../Halogen.HTML.Events.Types");
var TagName = function (x) {
    return x;
};
var PropName = function (x) {
    return x;
};
var Namespace = function (x) {
    return x;
};
var EventName = function (x) {
    return x;
};
var HandlerF = (function () {
    function HandlerF(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    HandlerF.create = function (value0) {
        return function (value1) {
            return new HandlerF(value0, value1);
        };
    };
    return HandlerF;
})();
var ClassName = function (x) {
    return x;
};
var AttrName = function (x) {
    return x;
};
var PropF = (function () {
    function PropF(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    PropF.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new PropF(value0, value1, value2);
            };
        };
    };
    return PropF;
})();
var Prop = (function () {
    function Prop(value0) {
        this.value0 = value0;
    };
    Prop.create = function (value0) {
        return new Prop(value0);
    };
    return Prop;
})();
var Attr = (function () {
    function Attr(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    Attr.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new Attr(value0, value1, value2);
            };
        };
    };
    return Attr;
})();
var Key = (function () {
    function Key(value0) {
        this.value0 = value0;
    };
    Key.create = function (value0) {
        return new Key(value0);
    };
    return Key;
})();
var Handler = (function () {
    function Handler(value0) {
        this.value0 = value0;
    };
    Handler.create = function (value0) {
        return new Handler(value0);
    };
    return Handler;
})();
var Ref = (function () {
    function Ref(value0) {
        this.value0 = value0;
    };
    Ref.create = function (value0) {
        return new Ref(value0);
    };
    return Ref;
})();
var Text = (function () {
    function Text(value0) {
        this.value0 = value0;
    };
    Text.create = function (value0) {
        return new Text(value0);
    };
    return Text;
})();
var Element = (function () {
    function Element(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    Element.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new Element(value0, value1, value2, value3);
                };
            };
        };
    };
    return Element;
})();
var Slot = (function () {
    function Slot(value0) {
        this.value0 = value0;
    };
    Slot.create = function (value0) {
        return new Slot(value0);
    };
    return Slot;
})();
var IsProp = function (toPropString) {
    this.toPropString = toPropString;
};
var toPropString = function (dict) {
    return dict.toPropString;
};
var tagName = TagName;
var stringIsProp = new IsProp(function (v) {
    return function (v1) {
        return function (s) {
            return s;
        };
    };
});
var runTagName = function (v) {
    return v;
};
var runPropName = function (v) {
    return v;
};
var runNamespace = function (v) {
    return v;
};
var runEventName = function (v) {
    return v;
};
var runClassName = function (v) {
    return v;
};
var runAttrName = function (v) {
    return v;
};
var propName = PropName;
var prop = function (dictIsProp) {
    return function (name) {
        return function (attr) {
            return function (v) {
                return new Prop(Data_Exists.mkExists(new PropF(name, v, Prelude["<$>"](Data_Maybe.functorMaybe)(Prelude.flip(Data_Tuple.Tuple.create)(toPropString(dictIsProp)))(attr))));
            };
        };
    };
};
var numberIsProp = new IsProp(function (v) {
    return function (v1) {
        return function (n) {
            return Prelude.show(Prelude.showNumber)(n);
        };
    };
});
var namespace = Namespace;
var intIsProp = new IsProp(function (v) {
    return function (v1) {
        return function (i) {
            return Prelude.show(Prelude.showInt)(i);
        };
    };
});
var handler = function (name) {
    return function (k) {
        return new Handler(Data_ExistsR.mkExistsR(new HandlerF(name, k)));
    };
};
var functorProp = new Prelude.Functor(function (v) {
    return function (v1) {
        if (v1 instanceof Prop) {
            return new Prop(v1.value0);
        };
        if (v1 instanceof Key) {
            return new Key(v1.value0);
        };
        if (v1 instanceof Attr) {
            return new Attr(v1.value0, v1.value1, v1.value2);
        };
        if (v1 instanceof Handler) {
            return Data_ExistsR.runExistsR(function (v2) {
                return new Handler(Data_ExistsR.mkExistsR(new HandlerF(v2.value0, function ($61) {
                    return Prelude.map(Halogen_HTML_Events_Handler.functorEventHandler)(Prelude.map(Data_Maybe.functorMaybe)(v))(v2.value1($61));
                })));
            })(v1.value0);
        };
        if (v1 instanceof Ref) {
            return new Ref(function ($62) {
                return v(v1.value0($62));
            });
        };
        throw new Error("Failed pattern match at Halogen.HTML.Core line 98, column 3 - line 99, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var fillSlot = function (dictApplicative) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof Text) {
                    return Prelude.pure(dictApplicative)(new Text(v2.value0));
                };
                if (v2 instanceof Element) {
                    return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Element.create(v2.value0)(v2.value1)(Prelude["<$>"](Prelude.functorArray)(Prelude.map(functorProp)(v1))(v2.value2)))(Data_Traversable.traverse(Data_Traversable.traversableArray)(dictApplicative)(fillSlot(dictApplicative)(v)(v1))(v2.value3));
                };
                if (v2 instanceof Slot) {
                    return v(v2.value0);
                };
                throw new Error("Failed pattern match at Halogen.HTML.Core line 77, column 1 - line 78, column 1: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    };
};
var eventName = EventName;
var element = Element.create(Data_Maybe.Nothing.value);
var className = ClassName;
var booleanIsProp = new IsProp(function (v) {
    return function (v1) {
        return function (v2) {
            if (v2) {
                return runAttrName(v);
            };
            if (!v2) {
                return "";
            };
            throw new Error("Failed pattern match at Halogen.HTML.Core line 138, column 3 - line 139, column 3: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
});
var bifunctorHTML = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        var go = function (v) {
            if (v instanceof Text) {
                return new Text(v.value0);
            };
            if (v instanceof Element) {
                return new Element(v.value0, v.value1, Prelude["<$>"](Prelude.functorArray)(Prelude.map(functorProp)(g))(v.value2), Prelude["<$>"](Prelude.functorArray)(go)(v.value3));
            };
            if (v instanceof Slot) {
                return new Slot(f(v.value0));
            };
            throw new Error("Failed pattern match at Halogen.HTML.Core line 62, column 3 - line 68, column 1: " + [ v.constructor.name ]);
        };
        return go;
    };
});
var functorHTML = new Prelude.Functor(Data_Bifunctor.rmap(bifunctorHTML));
var attrName = AttrName;
module.exports = {
    HandlerF: HandlerF, 
    PropF: PropF, 
    Prop: Prop, 
    Attr: Attr, 
    Key: Key, 
    Handler: Handler, 
    Ref: Ref, 
    Text: Text, 
    Element: Element, 
    Slot: Slot, 
    IsProp: IsProp, 
    runClassName: runClassName, 
    className: className, 
    runEventName: runEventName, 
    eventName: eventName, 
    runAttrName: runAttrName, 
    attrName: attrName, 
    runPropName: runPropName, 
    propName: propName, 
    runTagName: runTagName, 
    tagName: tagName, 
    runNamespace: runNamespace, 
    namespace: namespace, 
    toPropString: toPropString, 
    handler: handler, 
    prop: prop, 
    fillSlot: fillSlot, 
    element: element, 
    bifunctorHTML: bifunctorHTML, 
    functorHTML: functorHTML, 
    functorProp: functorProp, 
    stringIsProp: stringIsProp, 
    intIsProp: intIsProp, 
    numberIsProp: numberIsProp, 
    booleanIsProp: booleanIsProp
};

},{"../DOM.HTML.Types":87,"../Data.Bifunctor":104,"../Data.Exists":117,"../Data.ExistsR":118,"../Data.Maybe":152,"../Data.Traversable":170,"../Data.Tuple":171,"../Halogen.HTML.Events.Handler":184,"../Halogen.HTML.Events.Types":185,"../Prelude":203}],181:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_HTML_Properties_Indexed = require("../Halogen.HTML.Properties.Indexed");
var Halogen_HTML_Elements_1 = require("../Halogen.HTML.Elements");
var Halogen_HTML_Elements_1 = require("../Halogen.HTML.Elements");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var wbr = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.wbr);
var video = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.video);
var $$var = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1["var"]);
var ul = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.ul);
var u = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.u);
var tt = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.tt);
var track = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.track);
var tr = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.tr);
var title = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.title);
var time = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.time);
var thead = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.thead);
var th = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.th);
var tfoot = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.tfoot);
var textarea = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.textarea);
var td = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.td);
var tbody = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.tbody);
var table = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.table);
var sup = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.sup);
var summary = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.summary);
var sub = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.sub);
var style = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.style);
var strong = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.strong);
var span = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.span);
var source = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.source);
var small = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.small);
var select = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.select);
var section = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.section);
var script = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.script);
var samp = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.samp);
var s = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.s);
var ruby = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.ruby);
var rt = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.rt);
var rp = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.rp);
var q = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.q);
var progress = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.progress);
var pre = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.pre);
var param = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.param);
var p = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.p);
var output = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.output);
var option = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.option);
var optgroup = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.optgroup);
var ol = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.ol);
var object = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.object);
var noscript = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.noscript);
var noframes = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.noframes);
var nav = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.nav);
var meter = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.meter);
var meta = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.meta);
var menuitem = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.menuitem);
var menu = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.menu);
var mark = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.mark);
var map = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.map);
var main = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.main);
var link = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.link);
var li = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.li);
var legend = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.legend);
var label = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.label);
var keygen = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.keygen);
var kbd = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.kbd);
var ins = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.ins);
var input = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.input);
var img = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.img);
var iframe = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.iframe);
var i = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.i);
var html = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.html);
var hr = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.hr);
var header = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.header);
var head = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.head);
var h6 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.h6);
var h5 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.h5);
var h4 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.h4);
var h3 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.h3);
var h2 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.h2);
var h1 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.h1);
var form = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.form);
var footer = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.footer);
var figure = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.figure);
var figcaption = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.figcaption);
var fieldset = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.fieldset);
var embed = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.embed);
var em = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.em);
var dt = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.dt);
var dl = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.dl);
var div = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.div);
var dir = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.dir);
var dialog = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.dialog);
var dfn = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.dfn);
var details = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.details);
var del = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.del);
var dd = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.dd);
var datalist = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.datalist);
var command = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.command);
var colgroup = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.colgroup);
var col = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.col);
var code = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.code);
var cite = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.cite);
var center = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.center);
var caption = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.caption);
var canvas = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.canvas);
var button = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.button);
var br = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.br);
var body = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.body);
var blockquote = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.blockquote);
var big = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.big);
var bdo = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.bdo);
var bdi = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.bdi);
var basefont = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.basefont);
var base = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.base);
var b = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.b);
var audio = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.audio);
var aside = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.aside);
var article = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.article);
var area = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.area);
var applet = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.applet);
var address = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.address);
var acronym = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.acronym);
var abbr = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.a);
var a = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements_1.a);
module.exports = {
    wbr: wbr, 
    video: video, 
    "var": $$var, 
    ul: ul, 
    u: u, 
    tt: tt, 
    track: track, 
    tr: tr, 
    title: title, 
    time: time, 
    thead: thead, 
    th: th, 
    tfoot: tfoot, 
    textarea: textarea, 
    td: td, 
    tbody: tbody, 
    table: table, 
    sup: sup, 
    summary: summary, 
    sub: sub, 
    style: style, 
    strong: strong, 
    span: span, 
    source: source, 
    small: small, 
    select: select, 
    section: section, 
    script: script, 
    samp: samp, 
    s: s, 
    ruby: ruby, 
    rt: rt, 
    rp: rp, 
    q: q, 
    progress: progress, 
    pre: pre, 
    param: param, 
    p: p, 
    output: output, 
    option: option, 
    optgroup: optgroup, 
    ol: ol, 
    object: object, 
    noscript: noscript, 
    noframes: noframes, 
    nav: nav, 
    meter: meter, 
    meta: meta, 
    menuitem: menuitem, 
    menu: menu, 
    mark: mark, 
    map: map, 
    main: main, 
    link: link, 
    li: li, 
    legend: legend, 
    label: label, 
    keygen: keygen, 
    kbd: kbd, 
    ins: ins, 
    input: input, 
    img: img, 
    iframe: iframe, 
    i: i, 
    html: html, 
    hr: hr, 
    header: header, 
    head: head, 
    h6: h6, 
    h5: h5, 
    h4: h4, 
    h3: h3, 
    h2: h2, 
    h1: h1, 
    form: form, 
    footer: footer, 
    figure: figure, 
    figcaption: figcaption, 
    fieldset: fieldset, 
    embed: embed, 
    em: em, 
    dt: dt, 
    dl: dl, 
    div: div, 
    dir: dir, 
    dialog: dialog, 
    dfn: dfn, 
    details: details, 
    del: del, 
    dd: dd, 
    datalist: datalist, 
    command: command, 
    colgroup: colgroup, 
    col: col, 
    code: code, 
    cite: cite, 
    center: center, 
    caption: caption, 
    canvas: canvas, 
    button: button, 
    br: br, 
    body: body, 
    blockquote: blockquote, 
    big: big, 
    bdo: bdo, 
    bdi: bdi, 
    basefont: basefont, 
    base: base, 
    b: b, 
    audio: audio, 
    aside: aside, 
    article: article, 
    area: area, 
    applet: applet, 
    address: address, 
    acronym: acronym, 
    abbr: abbr, 
    a: a
};

},{"../Halogen.HTML.Core":180,"../Halogen.HTML.Elements":182,"../Halogen.HTML.Properties.Indexed":187,"../Unsafe.Coerce":206}],182:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var wbr = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("wbr"))(props)([  ]);
};
var video = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("video"))(xs);
};
var video_ = video([  ]);
var $$var = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("var"))(xs);
};
var var_ = $$var([  ]);
var ul = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("ul"))(xs);
};
var ul_ = ul([  ]);
var u = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("u"))(xs);
};
var u_ = u([  ]);
var tt = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("tt"))(xs);
};
var tt_ = tt([  ]);
var track = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("track"))(props)([  ]);
};
var tr = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("tr"))(xs);
};
var tr_ = tr([  ]);
var title = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("title"))(xs);
};
var title_ = title([  ]);
var time = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("time"))(xs);
};
var time_ = time([  ]);
var thead = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("thead"))(xs);
};
var thead_ = thead([  ]);
var th = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("th"))(xs);
};
var th_ = th([  ]);
var tfoot = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("tfoot"))(xs);
};
var tfoot_ = tfoot([  ]);
var textarea = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("textarea"))(xs)([  ]);
};
var td = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("td"))(xs);
};
var td_ = td([  ]);
var tbody = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("tbody"))(xs);
};
var tbody_ = tbody([  ]);
var table = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("table"))(xs);
};
var table_ = table([  ]);
var sup = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("sup"))(xs);
};
var sup_ = sup([  ]);
var summary = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("summary"))(xs);
};
var summary_ = summary([  ]);
var sub = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("sub"))(xs);
};
var sub_ = sub([  ]);
var style = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("style"))(xs);
};
var style_ = style([  ]);
var strong = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("strong"))(xs);
};
var strong_ = strong([  ]);
var strike = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("strike"))(xs);
};
var strike_ = strike([  ]);
var span = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("span"))(xs);
};
var span_ = span([  ]);
var source = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("source"))(props)([  ]);
};
var small = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("small"))(xs);
};
var small_ = small([  ]);
var select = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("select"))(xs);
};
var select_ = select([  ]);
var section = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("section"))(xs);
};
var section_ = section([  ]);
var script = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("script"))(xs);
};
var script_ = script([  ]);
var samp = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("samp"))(xs);
};
var samp_ = samp([  ]);
var s = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("s"))(xs);
};
var s_ = s([  ]);
var ruby = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("ruby"))(xs);
};
var ruby_ = ruby([  ]);
var rt = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("rt"))(xs);
};
var rt_ = rt([  ]);
var rp = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("rp"))(xs);
};
var rp_ = rp([  ]);
var q = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("q"))(xs);
};
var q_ = q([  ]);
var progress = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("progress"))(xs);
};
var progress_ = progress([  ]);
var pre = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("pre"))(xs);
};
var pre_ = pre([  ]);
var param = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("param"))(props)([  ]);
};
var p = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("p"))(xs);
};
var p_ = p([  ]);
var output = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("output"))(xs);
};
var output_ = output([  ]);
var option = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("option"))(xs);
};
var option_ = option([  ]);
var optgroup = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("optgroup"))(xs);
};
var optgroup_ = optgroup([  ]);
var ol = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("ol"))(xs);
};
var ol_ = ol([  ]);
var object = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("object"))(xs);
};
var object_ = object([  ]);
var noscript = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("noscript"))(xs);
};
var noscript_ = noscript([  ]);
var noframes = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("noframes"))(xs);
};
var noframes_ = noframes([  ]);
var nav = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("nav"))(xs);
};
var nav_ = nav([  ]);
var meter = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("meter"))(xs);
};
var meter_ = meter([  ]);
var meta = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("meta"))(props)([  ]);
};
var menuitem = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("menuitem"))(xs);
};
var menuitem_ = menuitem([  ]);
var menu = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("menu"))(xs);
};
var menu_ = menu([  ]);
var mark = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("mark"))(xs);
};
var mark_ = mark([  ]);
var map = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("map"))(xs);
};
var map_ = map([  ]);
var main = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("main"))(xs);
};
var main_ = main([  ]);
var link = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("link"))(props)([  ]);
};
var li = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("li"))(xs);
};
var li_ = li([  ]);
var legend = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("legend"))(xs);
};
var legend_ = legend([  ]);
var label = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("label"))(xs);
};
var label_ = label([  ]);
var keygen = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("keygen"))(props)([  ]);
};
var kbd = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("kbd"))(xs);
};
var kbd_ = kbd([  ]);
var ins = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("ins"))(xs);
};
var ins_ = ins([  ]);
var input = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("input"))(props)([  ]);
};
var img = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("img"))(props)([  ]);
};
var iframe = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("iframe"))(props)([  ]);
};
var i = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("i"))(xs);
};
var i_ = i([  ]);
var html = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("html"))(xs);
};
var html_ = html([  ]);
var hr = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("hr"))(props)([  ]);
};
var hr_ = hr([  ]);
var header = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("header"))(xs);
};
var header_ = header([  ]);
var head = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("head"))(xs);
};
var head_ = head([  ]);
var h6 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h6"))(xs);
};
var h6_ = h6([  ]);
var h5 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h5"))(xs);
};
var h5_ = h5([  ]);
var h4 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h4"))(xs);
};
var h4_ = h4([  ]);
var h3 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h3"))(xs);
};
var h3_ = h3([  ]);
var h2 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h2"))(xs);
};
var h2_ = h2([  ]);
var h1 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h1"))(xs);
};
var h1_ = h1([  ]);
var frameset = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("frameset"))(xs);
};
var frameset_ = frameset([  ]);
var frame = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("frame"))(xs);
};
var frame_ = frame([  ]);
var form = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("form"))(xs);
};
var form_ = form([  ]);
var footer = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("footer"))(xs);
};
var footer_ = footer([  ]);
var font = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("font"))(xs);
};
var font_ = font([  ]);
var figure = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("figure"))(xs);
};
var figure_ = figure([  ]);
var figcaption = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("figcaption"))(xs);
};
var figcaption_ = figcaption([  ]);
var fieldset = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("fieldset"))(xs);
};
var fieldset_ = fieldset([  ]);
var embed = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("embed"))(xs);
};
var embed_ = embed([  ]);
var em = Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("em"));
var em_ = em([  ]);
var dt = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dt"))(xs);
};
var dt_ = dt([  ]);
var dl = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dl"))(xs);
};
var dl_ = dl([  ]);
var div = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("div"))(xs);
};
var div_ = div([  ]);
var dir = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dir"))(xs);
};
var dir_ = dir([  ]);
var dialog = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dialog"))(xs);
};
var dialog_ = dialog([  ]);
var dfn = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dfn"))(xs);
};
var dfn_ = dfn([  ]);
var details = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("details"))(xs);
};
var details_ = details([  ]);
var del = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("del"))(xs);
};
var del_ = del([  ]);
var dd = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dd"))(xs);
};
var dd_ = dd([  ]);
var datalist = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("datalist"))(xs);
};
var datalist_ = datalist([  ]);
var command = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("command"))(props)([  ]);
};
var colgroup = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("colgroup"))(xs);
};
var colgroup_ = colgroup([  ]);
var col = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("col"))(props)([  ]);
};
var code = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("code"))(xs);
};
var code_ = code([  ]);
var cite = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("cite"))(xs);
};
var cite_ = cite([  ]);
var center = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("center"))(xs);
};
var center_ = center([  ]);
var caption = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("caption"))(xs);
};
var caption_ = caption([  ]);
var canvas = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("canvas"))(props)([  ]);
};
var button = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("button"))(xs);
};
var button_ = button([  ]);
var br = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("br"))(props)([  ]);
};
var br_ = br([  ]);
var body = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("body"))(xs);
};
var body_ = body([  ]);
var blockquote = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("blockquote"))(xs);
};
var blockquote_ = blockquote([  ]);
var big = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("big"))(xs);
};
var big_ = big([  ]);
var bdo = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("bdo"))(xs);
};
var bdo_ = bdo([  ]);
var bdi = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("bdi"))(xs);
};
var bdi_ = bdi([  ]);
var basefont = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("basefont"))(xs);
};
var basefont_ = basefont([  ]);
var base = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("base"))(props)([  ]);
};
var b = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("b"))(xs);
};
var b_ = b([  ]);
var audio = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("audio"))(xs);
};
var audio_ = audio([  ]);
var aside = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("aside"))(xs);
};
var aside_ = aside([  ]);
var article = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("article"))(xs);
};
var article_ = article([  ]);
var area = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("area"))(props)([  ]);
};
var applet = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("applet"))(xs);
};
var applet_ = applet([  ]);
var address = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("address"))(xs);
};
var address_ = address([  ]);
var acronym = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("acronym"))(xs);
};
var acronym_ = acronym([  ]);
var abbr = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("abbr"))(xs);
};
var abbr_ = abbr([  ]);
var a = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("a"))(xs);
};
var a_ = a([  ]);
module.exports = {
    wbr: wbr, 
    video_: video_, 
    video: video, 
    var_: var_, 
    "var": $$var, 
    ul_: ul_, 
    ul: ul, 
    u_: u_, 
    u: u, 
    tt_: tt_, 
    tt: tt, 
    track: track, 
    tr_: tr_, 
    tr: tr, 
    title_: title_, 
    title: title, 
    time_: time_, 
    time: time, 
    thead_: thead_, 
    thead: thead, 
    th_: th_, 
    th: th, 
    tfoot_: tfoot_, 
    tfoot: tfoot, 
    textarea: textarea, 
    td_: td_, 
    td: td, 
    tbody_: tbody_, 
    tbody: tbody, 
    table_: table_, 
    table: table, 
    sup_: sup_, 
    sup: sup, 
    summary_: summary_, 
    summary: summary, 
    sub_: sub_, 
    sub: sub, 
    style_: style_, 
    style: style, 
    strong_: strong_, 
    strong: strong, 
    strike_: strike_, 
    strike: strike, 
    span_: span_, 
    span: span, 
    source: source, 
    small_: small_, 
    small: small, 
    select_: select_, 
    select: select, 
    section_: section_, 
    section: section, 
    script_: script_, 
    script: script, 
    samp_: samp_, 
    samp: samp, 
    s_: s_, 
    s: s, 
    ruby_: ruby_, 
    ruby: ruby, 
    rt_: rt_, 
    rt: rt, 
    rp_: rp_, 
    rp: rp, 
    q_: q_, 
    q: q, 
    progress_: progress_, 
    progress: progress, 
    pre_: pre_, 
    pre: pre, 
    param: param, 
    p_: p_, 
    p: p, 
    output_: output_, 
    output: output, 
    option_: option_, 
    option: option, 
    optgroup_: optgroup_, 
    optgroup: optgroup, 
    ol_: ol_, 
    ol: ol, 
    object_: object_, 
    object: object, 
    noscript_: noscript_, 
    noscript: noscript, 
    noframes_: noframes_, 
    noframes: noframes, 
    nav_: nav_, 
    nav: nav, 
    meter_: meter_, 
    meter: meter, 
    meta: meta, 
    menuitem_: menuitem_, 
    menuitem: menuitem, 
    menu_: menu_, 
    menu: menu, 
    mark_: mark_, 
    mark: mark, 
    map_: map_, 
    map: map, 
    main_: main_, 
    main: main, 
    link: link, 
    li_: li_, 
    li: li, 
    legend_: legend_, 
    legend: legend, 
    label_: label_, 
    label: label, 
    keygen: keygen, 
    kbd_: kbd_, 
    kbd: kbd, 
    ins_: ins_, 
    ins: ins, 
    input: input, 
    img: img, 
    iframe: iframe, 
    i_: i_, 
    i: i, 
    html_: html_, 
    html: html, 
    hr_: hr_, 
    hr: hr, 
    header_: header_, 
    header: header, 
    head_: head_, 
    head: head, 
    h6_: h6_, 
    h6: h6, 
    h5_: h5_, 
    h5: h5, 
    h4_: h4_, 
    h4: h4, 
    h3_: h3_, 
    h3: h3, 
    h2_: h2_, 
    h2: h2, 
    h1_: h1_, 
    h1: h1, 
    frameset_: frameset_, 
    frameset: frameset, 
    frame_: frame_, 
    frame: frame, 
    form_: form_, 
    form: form, 
    footer_: footer_, 
    footer: footer, 
    font_: font_, 
    font: font, 
    figure_: figure_, 
    figure: figure, 
    figcaption_: figcaption_, 
    figcaption: figcaption, 
    fieldset_: fieldset_, 
    fieldset: fieldset, 
    embed_: embed_, 
    embed: embed, 
    em_: em_, 
    em: em, 
    dt_: dt_, 
    dt: dt, 
    dl_: dl_, 
    dl: dl, 
    div_: div_, 
    div: div, 
    dir_: dir_, 
    dir: dir, 
    dialog_: dialog_, 
    dialog: dialog, 
    dfn_: dfn_, 
    dfn: dfn, 
    details_: details_, 
    details: details, 
    del_: del_, 
    del: del, 
    dd_: dd_, 
    dd: dd, 
    datalist_: datalist_, 
    datalist: datalist, 
    command: command, 
    colgroup_: colgroup_, 
    colgroup: colgroup, 
    col: col, 
    code_: code_, 
    code: code, 
    cite_: cite_, 
    cite: cite, 
    center_: center_, 
    center: center, 
    caption_: caption_, 
    caption: caption, 
    canvas: canvas, 
    button_: button_, 
    button: button, 
    br_: br_, 
    br: br, 
    body_: body_, 
    body: body, 
    blockquote_: blockquote_, 
    blockquote: blockquote, 
    big_: big_, 
    big: big, 
    bdo_: bdo_, 
    bdo: bdo, 
    bdi_: bdi_, 
    bdi: bdi, 
    basefont_: basefont_, 
    basefont: basefont, 
    base: base, 
    b_: b_, 
    b: b, 
    audio_: audio_, 
    audio: audio, 
    aside_: aside_, 
    aside: aside, 
    article_: article_, 
    article: article, 
    area: area, 
    applet_: applet_, 
    applet: applet, 
    address_: address_, 
    address: address, 
    acronym_: acronym_, 
    acronym: acronym, 
    abbr_: abbr_, 
    abbr: abbr, 
    a_: a_, 
    a: a
};

},{"../Halogen.HTML.Core":180,"../Prelude":203}],183:[function(require,module,exports){
/* global exports */
"use strict";

// module Halogen.HTML.Events.Handler

exports.preventDefaultImpl = function (e) {
  return function () {
    e.preventDefault();
  };
};

exports.stopPropagationImpl = function (e) {
  return function () {
    e.stopPropagation();
  };
};

exports.stopImmediatePropagationImpl = function (e) {
  return function () {
    e.stopImmediatePropagation();
  };
};

},{}],184:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Apply = require("../Control.Apply");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Writer = require("../Control.Monad.Writer");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Data_Foldable = require("../Data.Foldable");
var Data_Tuple = require("../Data.Tuple");
var DOM = require("../DOM");
var Halogen_HTML_Events_Types = require("../Halogen.HTML.Events.Types");
var Data_Monoid = require("../Data.Monoid");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans");
var Data_Identity = require("../Data.Identity");
var PreventDefault = (function () {
    function PreventDefault() {

    };
    PreventDefault.value = new PreventDefault();
    return PreventDefault;
})();
var StopPropagation = (function () {
    function StopPropagation() {

    };
    StopPropagation.value = new StopPropagation();
    return StopPropagation;
})();
var StopImmediatePropagation = (function () {
    function StopImmediatePropagation() {

    };
    StopImmediatePropagation.value = new StopImmediatePropagation();
    return StopImmediatePropagation;
})();
var EventHandler = function (x) {
    return x;
};
var unEventHandler = function (v) {
    return v;
};
var stopPropagation = Control_Monad_Writer_Class.tell(Data_Monoid.monoidArray)(Control_Monad_Writer_Trans.monadWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))(Control_Monad_Writer_Trans.monadWriterWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))([ StopPropagation.value ]);
var stopImmediatePropagation = Control_Monad_Writer_Class.tell(Data_Monoid.monoidArray)(Control_Monad_Writer_Trans.monadWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))(Control_Monad_Writer_Trans.monadWriterWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))([ StopImmediatePropagation.value ]);
var runEventHandler = function (dictMonad) {
    return function (dictMonadEff) {
        return function (e) {
            return function (v) {
                var applyUpdate = function (v1) {
                    if (v1 instanceof PreventDefault) {
                        return $foreign.preventDefaultImpl(e);
                    };
                    if (v1 instanceof StopPropagation) {
                        return $foreign.stopPropagationImpl(e);
                    };
                    if (v1 instanceof StopImmediatePropagation) {
                        return $foreign.stopImmediatePropagationImpl(e);
                    };
                    throw new Error("Failed pattern match at Halogen.HTML.Events.Handler line 89, column 3 - line 90, column 3: " + [ v1.constructor.name ]);
                };
                var $13 = Control_Monad_Writer.runWriter(v);
                return Control_Monad_Eff_Class.liftEff(dictMonadEff)(Control_Apply["*>"](Control_Monad_Eff.applyEff)(Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)($13.value1)(applyUpdate))(Prelude["return"](Control_Monad_Eff.applicativeEff)($13.value0)));
            };
        };
    };
};
var preventDefault = Control_Monad_Writer_Class.tell(Data_Monoid.monoidArray)(Control_Monad_Writer_Trans.monadWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))(Control_Monad_Writer_Trans.monadWriterWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))([ PreventDefault.value ]);
var functorEventHandler = new Prelude.Functor(function (f) {
    return function (v) {
        return Prelude["<$>"](Control_Monad_Writer_Trans.functorWriterT(Data_Identity.functorIdentity))(f)(v);
    };
});
var applyEventHandler = new Prelude.Apply(function () {
    return functorEventHandler;
}, function (v) {
    return function (v1) {
        return Prelude["<*>"](Control_Monad_Writer_Trans.applyWriterT(Prelude.semigroupArray)(Data_Identity.applyIdentity))(v)(v1);
    };
});
var bindEventHandler = new Prelude.Bind(function () {
    return applyEventHandler;
}, function (v) {
    return function (f) {
        return Prelude[">>="](Control_Monad_Writer_Trans.bindWriterT(Prelude.semigroupArray)(Data_Identity.monadIdentity))(v)(function ($22) {
            return unEventHandler(f($22));
        });
    };
});
var applicativeEventHandler = new Prelude.Applicative(function () {
    return applyEventHandler;
}, function ($23) {
    return EventHandler(Prelude.pure(Control_Monad_Writer_Trans.applicativeWriterT(Data_Monoid.monoidArray)(Data_Identity.applicativeIdentity))($23));
});
var monadEventHandler = new Prelude.Monad(function () {
    return applicativeEventHandler;
}, function () {
    return bindEventHandler;
});
module.exports = {
    runEventHandler: runEventHandler, 
    stopImmediatePropagation: stopImmediatePropagation, 
    stopPropagation: stopPropagation, 
    preventDefault: preventDefault, 
    functorEventHandler: functorEventHandler, 
    applyEventHandler: applyEventHandler, 
    applicativeEventHandler: applicativeEventHandler, 
    bindEventHandler: bindEventHandler, 
    monadEventHandler: monadEventHandler
};

},{"../Control.Apply":28,"../Control.Monad.Eff":54,"../Control.Monad.Eff.Class":46,"../Control.Monad.Writer":74,"../Control.Monad.Writer.Class":72,"../Control.Monad.Writer.Trans":73,"../DOM":98,"../Data.Foldable":120,"../Data.Identity":137,"../Data.Monoid":159,"../Data.Tuple":171,"../Halogen.HTML.Events.Types":185,"../Prelude":203,"./foreign":183}],185:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Data_Nullable = require("../Data.Nullable");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_Event_DragEvent_DataTransfer = require("../DOM.Event.DragEvent.DataTransfer");
module.exports = {};

},{"../DOM.Event.DragEvent.DataTransfer":79,"../DOM.HTML.Types":87,"../Data.Nullable":162}],186:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Halogen_HTML = require("../Halogen.HTML");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_HTML_Elements_Indexed = require("../Halogen.HTML.Elements.Indexed");
module.exports = {};

},{"../Halogen.HTML":190,"../Halogen.HTML.Core":180,"../Halogen.HTML.Elements.Indexed":181}],187:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Array = require("../Data.Array");
var Data_Foldable = require("../Data.Foldable");
var Data_Maybe = require("../Data.Maybe");
var Data_Tuple = require("../Data.Tuple");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_HTML_Properties_1 = require("../Halogen.HTML.Properties");
var Halogen_HTML_Properties_1 = require("../Halogen.HTML.Properties");
var Data_Monoid = require("../Data.Monoid");
var MenuitemCommand = (function () {
    function MenuitemCommand() {

    };
    MenuitemCommand.value = new MenuitemCommand();
    return MenuitemCommand;
})();
var MenuitemCheckbox = (function () {
    function MenuitemCheckbox() {

    };
    MenuitemCheckbox.value = new MenuitemCheckbox();
    return MenuitemCheckbox;
})();
var MenuitemRadio = (function () {
    function MenuitemRadio() {

    };
    MenuitemRadio.value = new MenuitemRadio();
    return MenuitemRadio;
})();
var MenuList = (function () {
    function MenuList() {

    };
    MenuList.value = new MenuList();
    return MenuList;
})();
var MenuContext = (function () {
    function MenuContext() {

    };
    MenuContext.value = new MenuContext();
    return MenuContext;
})();
var MenuToolbar = (function () {
    function MenuToolbar() {

    };
    MenuToolbar.value = new MenuToolbar();
    return MenuToolbar;
})();
var InputButton = (function () {
    function InputButton() {

    };
    InputButton.value = new InputButton();
    return InputButton;
})();
var InputCheckbox = (function () {
    function InputCheckbox() {

    };
    InputCheckbox.value = new InputCheckbox();
    return InputCheckbox;
})();
var InputColor = (function () {
    function InputColor() {

    };
    InputColor.value = new InputColor();
    return InputColor;
})();
var InputDate = (function () {
    function InputDate() {

    };
    InputDate.value = new InputDate();
    return InputDate;
})();
var InputDatetime = (function () {
    function InputDatetime() {

    };
    InputDatetime.value = new InputDatetime();
    return InputDatetime;
})();
var InputDatetimeLocal = (function () {
    function InputDatetimeLocal() {

    };
    InputDatetimeLocal.value = new InputDatetimeLocal();
    return InputDatetimeLocal;
})();
var InputEmail = (function () {
    function InputEmail() {

    };
    InputEmail.value = new InputEmail();
    return InputEmail;
})();
var InputFile = (function () {
    function InputFile() {

    };
    InputFile.value = new InputFile();
    return InputFile;
})();
var InputHidden = (function () {
    function InputHidden() {

    };
    InputHidden.value = new InputHidden();
    return InputHidden;
})();
var InputImage = (function () {
    function InputImage() {

    };
    InputImage.value = new InputImage();
    return InputImage;
})();
var InputMonth = (function () {
    function InputMonth() {

    };
    InputMonth.value = new InputMonth();
    return InputMonth;
})();
var InputNumber = (function () {
    function InputNumber() {

    };
    InputNumber.value = new InputNumber();
    return InputNumber;
})();
var InputPassword = (function () {
    function InputPassword() {

    };
    InputPassword.value = new InputPassword();
    return InputPassword;
})();
var InputRadio = (function () {
    function InputRadio() {

    };
    InputRadio.value = new InputRadio();
    return InputRadio;
})();
var InputRange = (function () {
    function InputRange() {

    };
    InputRange.value = new InputRange();
    return InputRange;
})();
var InputReset = (function () {
    function InputReset() {

    };
    InputReset.value = new InputReset();
    return InputReset;
})();
var InputSearch = (function () {
    function InputSearch() {

    };
    InputSearch.value = new InputSearch();
    return InputSearch;
})();
var InputSubmit = (function () {
    function InputSubmit() {

    };
    InputSubmit.value = new InputSubmit();
    return InputSubmit;
})();
var InputTel = (function () {
    function InputTel() {

    };
    InputTel.value = new InputTel();
    return InputTel;
})();
var InputText = (function () {
    function InputText() {

    };
    InputText.value = new InputText();
    return InputText;
})();
var InputTime = (function () {
    function InputTime() {

    };
    InputTime.value = new InputTime();
    return InputTime;
})();
var InputUrl = (function () {
    function InputUrl() {

    };
    InputUrl.value = new InputUrl();
    return InputUrl;
})();
var InputWeek = (function () {
    function InputWeek() {

    };
    InputWeek.value = new InputWeek();
    return InputWeek;
})();
var IProp = function (x) {
    return x;
};
var Uppercase = (function () {
    function Uppercase() {

    };
    Uppercase.value = new Uppercase();
    return Uppercase;
})();
var Lowercase = (function () {
    function Lowercase() {

    };
    Lowercase.value = new Lowercase();
    return Lowercase;
})();
var NumeralDecimal = (function () {
    function NumeralDecimal() {

    };
    NumeralDecimal.value = new NumeralDecimal();
    return NumeralDecimal;
})();
var NumeralRoman = (function () {
    function NumeralRoman(value0) {
        this.value0 = value0;
    };
    NumeralRoman.create = function (value0) {
        return new NumeralRoman(value0);
    };
    return NumeralRoman;
})();
var OrderedListNumeric = (function () {
    function OrderedListNumeric(value0) {
        this.value0 = value0;
    };
    OrderedListNumeric.create = function (value0) {
        return new OrderedListNumeric(value0);
    };
    return OrderedListNumeric;
})();
var OrderedListAlphabetic = (function () {
    function OrderedListAlphabetic(value0) {
        this.value0 = value0;
    };
    OrderedListAlphabetic.create = function (value0) {
        return new OrderedListAlphabetic(value0);
    };
    return OrderedListAlphabetic;
})();
var ButtonButton = (function () {
    function ButtonButton() {

    };
    ButtonButton.value = new ButtonButton();
    return ButtonButton;
})();
var ButtonSubmit = (function () {
    function ButtonSubmit() {

    };
    ButtonSubmit.value = new ButtonSubmit();
    return ButtonSubmit;
})();
var ButtonReset = (function () {
    function ButtonReset() {

    };
    ButtonReset.value = new ButtonReset();
    return ButtonReset;
})();
var renderOrderedListType = function (ty) {
    if (ty instanceof OrderedListNumeric) {
        if (ty.value0 instanceof NumeralDecimal) {
            return "1";
        };
        if (ty.value0 instanceof NumeralRoman) {
            if (ty.value0.value0 instanceof Lowercase) {
                return "i";
            };
            if (ty.value0.value0 instanceof Uppercase) {
                return "I";
            };
            throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 296, column 11 - line 299, column 5: " + [ ty.value0.value0.constructor.name ]);
        };
        throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 293, column 7 - line 299, column 5: " + [ ty.value0.constructor.name ]);
    };
    if (ty instanceof OrderedListAlphabetic) {
        if (ty.value0 instanceof Lowercase) {
            return "a";
        };
        if (ty.value0 instanceof Uppercase) {
            return "A";
        };
        throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 300, column 7 - line 304, column 1: " + [ ty.value0.constructor.name ]);
    };
    throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 291, column 3 - line 304, column 1: " + [ ty.constructor.name ]);
};
var renderMenuitemType = function (ty) {
    if (ty instanceof MenuitemCommand) {
        return "command";
    };
    if (ty instanceof MenuitemCheckbox) {
        return "checkbox";
    };
    if (ty instanceof MenuitemRadio) {
        return "radio";
    };
    throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 234, column 3 - line 239, column 1: " + [ ty.constructor.name ]);
};
var renderMenuType = function (ty) {
    if (ty instanceof MenuList) {
        return "list";
    };
    if (ty instanceof MenuContext) {
        return "context";
    };
    if (ty instanceof MenuToolbar) {
        return "toolbar";
    };
    throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 219, column 3 - line 224, column 1: " + [ ty.constructor.name ]);
};
var renderMediaType = function (ty) {
    var renderParameter = function (v) {
        return v.value0 + ("=" + v.value1);
    };
    var renderParameters = function (ps) {
        if (Data_Array.length(ps) === 0) {
            return "";
        };
        if (Prelude.otherwise) {
            return ";" + Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(";")(Prelude["<$>"](Prelude.functorArray)(renderParameter)(ps));
        };
        throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 252, column 5 - line 256, column 5: " + [ ps.constructor.name ]);
    };
    return ty.type + ("/" + (ty.subtype + renderParameters(ty.parameters)));
};
var renderInputType = function (ty) {
    if (ty instanceof InputButton) {
        return "button";
    };
    if (ty instanceof InputCheckbox) {
        return "checkbox";
    };
    if (ty instanceof InputColor) {
        return "color";
    };
    if (ty instanceof InputDate) {
        return "date";
    };
    if (ty instanceof InputDatetime) {
        return "datetime";
    };
    if (ty instanceof InputDatetimeLocal) {
        return "datetime-local";
    };
    if (ty instanceof InputEmail) {
        return "email";
    };
    if (ty instanceof InputFile) {
        return "file";
    };
    if (ty instanceof InputHidden) {
        return "hidden";
    };
    if (ty instanceof InputImage) {
        return "image";
    };
    if (ty instanceof InputMonth) {
        return "month";
    };
    if (ty instanceof InputNumber) {
        return "number";
    };
    if (ty instanceof InputPassword) {
        return "password";
    };
    if (ty instanceof InputRadio) {
        return "radio";
    };
    if (ty instanceof InputRange) {
        return "range";
    };
    if (ty instanceof InputReset) {
        return "reset";
    };
    if (ty instanceof InputSearch) {
        return "search";
    };
    if (ty instanceof InputSubmit) {
        return "submit";
    };
    if (ty instanceof InputTel) {
        return "tel";
    };
    if (ty instanceof InputText) {
        return "text";
    };
    if (ty instanceof InputTime) {
        return "time";
    };
    if (ty instanceof InputUrl) {
        return "url";
    };
    if (ty instanceof InputWeek) {
        return "week";
    };
    throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 184, column 3 - line 209, column 1: " + [ ty.constructor.name ]);
};
var renderButtonType = function (ty) {
    if (ty instanceof ButtonButton) {
        return "button";
    };
    if (ty instanceof ButtonSubmit) {
        return "submit";
    };
    if (ty instanceof ButtonReset) {
        return "reset";
    };
    throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 269, column 3 - line 274, column 1: " + [ ty.constructor.name ]);
};
var refine = Unsafe_Coerce.unsafeCoerce;
var rel = refine(Halogen_HTML_Properties_1.rel);
var required = refine(Halogen_HTML_Properties_1.required);
var rowSpan = refine(Halogen_HTML_Properties_1.rowSpan);
var rows = refine(Halogen_HTML_Properties_1.rows);
var selected = refine(Halogen_HTML_Properties_1.selected);
var spellcheck = refine(Halogen_HTML_Properties_1.spellcheck);
var src = refine(Halogen_HTML_Properties_1.src);
var tabIndex = refine(Halogen_HTML_Properties_1.tabIndex);
var target = refine(Halogen_HTML_Properties_1.target);
var title = refine(Halogen_HTML_Properties_1.title);
var value = refine(Halogen_HTML_Properties_1.value);
var width = refine(Halogen_HTML_Properties_1.width);
var ref = refine(Halogen_HTML_Properties_1.ref);
var readonly = refine(Halogen_HTML_Properties_1.readonly);
var placeholder = refine(Halogen_HTML_Properties_1.placeholder);
var olType = function ($16) {
    return refine(Halogen_HTML_Properties_1.type_)(renderOrderedListType($16));
};
var name = refine(Halogen_HTML_Properties_1.name);
var multiple = refine(Halogen_HTML_Properties_1.multiple);
var menuitemType = function ($17) {
    return refine(Halogen_HTML_Properties_1.type_)(renderMenuitemType($17));
};
var menuType = function ($18) {
    return refine(Halogen_HTML_Properties_1.type_)(renderMenuType($18));
};
var mediaType = function ($19) {
    return refine(Halogen_HTML_Properties_1.type_)(renderMediaType($19));
};
var key = refine(Halogen_HTML_Properties_1.key);
var inputType = function ($20) {
    return refine(Halogen_HTML_Properties_1.type_)(renderInputType($20));
};
var id_ = refine(Halogen_HTML_Properties_1.id_);
var href = refine(Halogen_HTML_Properties_1.href);
var height = refine(Halogen_HTML_Properties_1.height);
var $$for = refine(Halogen_HTML_Properties_1["for"]);
var draggable = refine(Halogen_HTML_Properties_1.draggable);
var disabled = refine(Halogen_HTML_Properties_1.disabled);
var enabled = function ($21) {
    return disabled(!$21);
};
var cols = refine(Halogen_HTML_Properties_1.cols);
var colSpan = refine(Halogen_HTML_Properties_1.colSpan);
var classes = refine(Halogen_HTML_Properties_1.classes);
var class_ = refine(Halogen_HTML_Properties_1.class_);
var checked = refine(Halogen_HTML_Properties_1.checked);
var charset = refine(Halogen_HTML_Properties_1.charset);
var buttonType = function ($22) {
    return refine(Halogen_HTML_Properties_1.type_)(renderButtonType($22));
};
var autofocus = refine(Halogen_HTML_Properties_1.autofocus);
var autocomplete = refine(Halogen_HTML_Properties_1.autocomplete);
var alt = refine(Halogen_HTML_Properties_1.alt);
module.exports = {
    Uppercase: Uppercase, 
    Lowercase: Lowercase, 
    NumeralDecimal: NumeralDecimal, 
    NumeralRoman: NumeralRoman, 
    OrderedListNumeric: OrderedListNumeric, 
    OrderedListAlphabetic: OrderedListAlphabetic, 
    MenuitemCommand: MenuitemCommand, 
    MenuitemCheckbox: MenuitemCheckbox, 
    MenuitemRadio: MenuitemRadio, 
    MenuList: MenuList, 
    MenuContext: MenuContext, 
    MenuToolbar: MenuToolbar, 
    InputButton: InputButton, 
    InputCheckbox: InputCheckbox, 
    InputColor: InputColor, 
    InputDate: InputDate, 
    InputDatetime: InputDatetime, 
    InputDatetimeLocal: InputDatetimeLocal, 
    InputEmail: InputEmail, 
    InputFile: InputFile, 
    InputHidden: InputHidden, 
    InputImage: InputImage, 
    InputMonth: InputMonth, 
    InputNumber: InputNumber, 
    InputPassword: InputPassword, 
    InputRadio: InputRadio, 
    InputRange: InputRange, 
    InputReset: InputReset, 
    InputSearch: InputSearch, 
    InputSubmit: InputSubmit, 
    InputTel: InputTel, 
    InputText: InputText, 
    InputTime: InputTime, 
    InputUrl: InputUrl, 
    InputWeek: InputWeek, 
    ButtonButton: ButtonButton, 
    ButtonSubmit: ButtonSubmit, 
    ButtonReset: ButtonReset, 
    IProp: IProp, 
    ref: ref, 
    tabIndex: tabIndex, 
    draggable: draggable, 
    multiple: multiple, 
    autofocus: autofocus, 
    autocomplete: autocomplete, 
    placeholder: placeholder, 
    selected: selected, 
    checked: checked, 
    spellcheck: spellcheck, 
    readonly: readonly, 
    required: required, 
    enabled: enabled, 
    disabled: disabled, 
    value: value, 
    olType: olType, 
    menuitemType: menuitemType, 
    menuType: menuType, 
    mediaType: mediaType, 
    inputType: inputType, 
    buttonType: buttonType, 
    title: title, 
    target: target, 
    src: src, 
    rel: rel, 
    name: name, 
    id_: id_, 
    href: href, 
    width: width, 
    height: height, 
    "for": $$for, 
    rowSpan: rowSpan, 
    colSpan: colSpan, 
    rows: rows, 
    cols: cols, 
    classes: classes, 
    class_: class_, 
    charset: charset, 
    alt: alt, 
    key: key
};

},{"../DOM.HTML.Types":87,"../Data.Array":102,"../Data.Foldable":120,"../Data.Maybe":152,"../Data.Monoid":159,"../Data.Tuple":171,"../Halogen.HTML.Core":180,"../Halogen.HTML.Properties":188,"../Prelude":203,"../Unsafe.Coerce":206}],188:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_String = require("../Data.String");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Pixels = (function () {
    function Pixels(value0) {
        this.value0 = value0;
    };
    Pixels.create = function (value0) {
        return new Pixels(value0);
    };
    return Pixels;
})();
var Percent = (function () {
    function Percent(value0) {
        this.value0 = value0;
    };
    Percent.create = function (value0) {
        return new Percent(value0);
    };
    return Percent;
})();
var value = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("value"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("value")));
var type_ = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("type"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("type")));
var title = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("title"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("title")));
var target = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("target"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("target")));
var tabIndex = Halogen_HTML_Core.prop(Halogen_HTML_Core.intIsProp)(Halogen_HTML_Core.propName("tabIndex"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("tabindex")));
var src = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("src"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("src")));
var spellcheck = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("spellcheck"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("spellcheck")));
var selected = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("selected"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("selected")));
var rows = Halogen_HTML_Core.prop(Halogen_HTML_Core.intIsProp)(Halogen_HTML_Core.propName("rows"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("rows")));
var rowSpan = Halogen_HTML_Core.prop(Halogen_HTML_Core.intIsProp)(Halogen_HTML_Core.propName("rowSpan"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("rowspan")));
var required = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("required"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("required")));
var rel = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("rel"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("rel")));
var ref = Halogen_HTML_Core.Ref.create;
var readonly = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("readOnly"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("readonly")));
var printLengthLiteral = function (v) {
    if (v instanceof Pixels) {
        return Prelude.show(Prelude.showInt)(v.value0);
    };
    if (v instanceof Percent) {
        return Prelude.show(Prelude.showNumber)(v.value0) + "%";
    };
    throw new Error("Failed pattern match at Halogen.HTML.Properties line 61, column 1 - line 62, column 1: " + [ v.constructor.name ]);
};
var width = function ($5) {
    return Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("width"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("width")))(printLengthLiteral($5));
};
var placeholder = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("placeholder"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("placeholder")));
var name = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("name"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("name")));
var multiple = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("multiple"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("multiple")));
var key = Halogen_HTML_Core.Key.create;
var id_ = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("id"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("id")));
var href = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("href"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("href")));
var height = function ($6) {
    return Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("height"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("height")))(printLengthLiteral($6));
};
var $$for = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("htmlFor"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("for")));
var draggable = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("draggable"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("draggable")));
var disabled = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("disabled"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("disabled")));
var enabled = function ($7) {
    return disabled(!$7);
};
var cols = Halogen_HTML_Core.prop(Halogen_HTML_Core.intIsProp)(Halogen_HTML_Core.propName("cols"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("cols")));
var colSpan = Halogen_HTML_Core.prop(Halogen_HTML_Core.intIsProp)(Halogen_HTML_Core.propName("colSpan"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("colspan")));
var classes = function ($8) {
    return Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("className"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("class")))(Data_String.joinWith(" ")(Prelude.map(Prelude.functorArray)(Halogen_HTML_Core.runClassName)($8)));
};
var class_ = function ($9) {
    return Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("className"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("class")))(Halogen_HTML_Core.runClassName($9));
};
var checked = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("checked"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("checked")));
var charset = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("charset"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("charset")));
var autofocus = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("autofocus"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("autofocus")));
var autocomplete = function ($10) {
    return Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("autocomplete"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("autocomplete")))((function (b) {
        if (b) {
            return "on";
        };
        if (!b) {
            return "off";
        };
        throw new Error("Failed pattern match at Halogen.HTML.Properties line 155, column 91 - line 155, column 116: " + [ b.constructor.name ]);
    })($10));
};
var alt = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("alt"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("alt")));
module.exports = {
    Pixels: Pixels, 
    Percent: Percent, 
    ref: ref, 
    tabIndex: tabIndex, 
    draggable: draggable, 
    multiple: multiple, 
    autofocus: autofocus, 
    autocomplete: autocomplete, 
    placeholder: placeholder, 
    selected: selected, 
    checked: checked, 
    spellcheck: spellcheck, 
    readonly: readonly, 
    required: required, 
    enabled: enabled, 
    disabled: disabled, 
    value: value, 
    type_: type_, 
    title: title, 
    target: target, 
    src: src, 
    rel: rel, 
    name: name, 
    id_: id_, 
    href: href, 
    width: width, 
    height: height, 
    "for": $$for, 
    rowSpan: rowSpan, 
    colSpan: colSpan, 
    rows: rows, 
    cols: cols, 
    classes: classes, 
    class_: class_, 
    charset: charset, 
    alt: alt, 
    key: key
};

},{"../DOM.HTML.Types":87,"../Data.Maybe":152,"../Data.String":168,"../Halogen.HTML.Core":180,"../Prelude":203}],189:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Data_Exists = require("../Data.Exists");
var Data_ExistsR = require("../Data.ExistsR");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Lazy = require("../Data.Lazy");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Nullable = require("../Data.Nullable");
var Halogen_Effects = require("../Halogen.Effects");
var Halogen_Component_Tree = require("../Halogen.Component.Tree");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_HTML_Events_Handler = require("../Halogen.HTML.Events.Handler");
var Halogen_Internal_VirtualDOM = require("../Halogen.Internal.VirtualDOM");
var handleAff = Control_Monad_Aff.runAff(Control_Monad_Eff_Exception.throwException)(Prelude["const"](Prelude.pure(Control_Monad_Eff.applicativeEff)(Prelude.unit)));
var renderProp = function (v) {
    return function (v1) {
        if (v1 instanceof Halogen_HTML_Core.Prop) {
            return Data_Exists.runExists(function (v2) {
                return Halogen_Internal_VirtualDOM.prop(Halogen_HTML_Core.runPropName(v2.value0), v2.value1);
            })(v1.value0);
        };
        if (v1 instanceof Halogen_HTML_Core.Attr) {
            var attrName = Data_Maybe.maybe("")(function (ns$prime) {
                return Halogen_HTML_Core.runNamespace(ns$prime) + ":";
            })(v1.value0) + Halogen_HTML_Core.runAttrName(v1.value1);
            return Halogen_Internal_VirtualDOM.attr(attrName, v1.value2);
        };
        if (v1 instanceof Halogen_HTML_Core.Handler) {
            return Data_ExistsR.runExistsR(function (v2) {
                return Halogen_Internal_VirtualDOM.handlerProp(Halogen_HTML_Core.runEventName(v2.value0), function (ev) {
                    return handleAff(Prelude[">>="](Control_Monad_Aff.bindAff)(Halogen_HTML_Events_Handler.runEventHandler(Control_Monad_Aff.monadAff)(Control_Monad_Aff.monadEffAff)(ev)(v2.value1(ev)))(Data_Maybe.maybe(Prelude.pure(Control_Monad_Aff.applicativeAff)(Prelude.unit))(v)));
                });
            })(v1.value0);
        };
        if (v1 instanceof Halogen_HTML_Core.Ref) {
            return Halogen_Internal_VirtualDOM.refProp(function ($40) {
                return handleAff(v(v1.value0($40)));
            });
        };
        return Data_Monoid.mempty(Halogen_Internal_VirtualDOM.monoidProps);
    };
};
var findKey = function (v) {
    return function (v1) {
        if (v1 instanceof Halogen_HTML_Core.Key) {
            return new Data_Maybe.Just(v1.value0);
        };
        return v;
    };
};
var renderHTML = function (f) {
    var go = function (v) {
        if (v instanceof Halogen_HTML_Core.Text) {
            return Halogen_Internal_VirtualDOM.vtext(v.value0);
        };
        if (v instanceof Halogen_HTML_Core.Element) {
            var tag = Halogen_HTML_Core.runTagName(v.value1);
            var ns$prime = Data_Nullable.toNullable(Prelude["<$>"](Data_Maybe.functorMaybe)(Halogen_HTML_Core.runNamespace)(v.value0));
            var key = Data_Nullable.toNullable(Data_Foldable.foldl(Data_Foldable.foldableArray)(findKey)(Data_Maybe.Nothing.value)(v.value2));
            return Halogen_Internal_VirtualDOM.vnode(ns$prime)(tag)(key)(Data_Foldable.foldMap(Data_Foldable.foldableArray)(Halogen_Internal_VirtualDOM.monoidProps)(renderProp(f))(v.value2))(Prelude.map(Prelude.functorArray)(go)(v.value3));
        };
        if (v instanceof Halogen_HTML_Core.Slot) {
            return Halogen_Internal_VirtualDOM.vtext("");
        };
        throw new Error("Failed pattern match at Halogen.HTML.Renderer.VirtualDOM line 34, column 3 - line 35, column 3: " + [ v.constructor.name ]);
    };
    return go;
};
var renderTree = function (f) {
    return Halogen_Component_Tree.runTree(function (tree) {
        var go = function (v) {
            if (v instanceof Halogen_HTML_Core.Text) {
                return Halogen_Internal_VirtualDOM.vtext(v.value0);
            };
            if (v instanceof Halogen_HTML_Core.Slot) {
                return Halogen_Internal_VirtualDOM.widget(v.value0)(tree.eq)(renderTree(f));
            };
            if (v instanceof Halogen_HTML_Core.Element) {
                var tag = Halogen_HTML_Core.runTagName(v.value1);
                var ns$prime = Data_Nullable.toNullable(Prelude["<$>"](Data_Maybe.functorMaybe)(Halogen_HTML_Core.runNamespace)(v.value0));
                var key = Data_Nullable.toNullable(Data_Foldable.foldl(Data_Foldable.foldableArray)(findKey)(Data_Maybe.Nothing.value)(v.value2));
                return Halogen_Internal_VirtualDOM.vnode(ns$prime)(tag)(key)(Data_Foldable.foldMap(Data_Foldable.foldableArray)(Halogen_Internal_VirtualDOM.monoidProps)(renderProp(f))(v.value2))(Prelude.map(Prelude.functorArray)(go)(v.value3));
            };
            throw new Error("Failed pattern match at Halogen.HTML.Renderer.VirtualDOM line 49, column 5 - line 58, column 1: " + [ v.constructor.name ]);
        };
        return go(Data_Lazy.force(tree.html));
    });
};
module.exports = {
    renderTree: renderTree, 
    renderHTML: renderHTML
};

},{"../Control.Monad.Aff":43,"../Control.Monad.Eff":54,"../Control.Monad.Eff.Exception":48,"../Data.Exists":117,"../Data.ExistsR":118,"../Data.Foldable":120,"../Data.Function":130,"../Data.Lazy":145,"../Data.Maybe":152,"../Data.Monoid":159,"../Data.Nullable":162,"../Halogen.Component.Tree":176,"../Halogen.Effects":179,"../Halogen.HTML.Core":180,"../Halogen.HTML.Events.Handler":184,"../Halogen.Internal.VirtualDOM":192,"../Prelude":203}],190:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Halogen_Component = require("../Halogen.Component");
var Halogen_Component_ChildPath = require("../Halogen.Component.ChildPath");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_HTML_Elements = require("../Halogen.HTML.Elements");
var text = Halogen_HTML_Core.Text.create;
var slot$prime = function (dictFunctor) {
    return function (i) {
        return function (p) {
            return function (l) {
                var transform = function (def) {
                    return {
                        component: Halogen_Component.transformChild(dictFunctor)(i)(def.component), 
                        initialState: Halogen_Component_ChildPath.injState(i)(def.initialState)
                    };
                };
                return new Halogen_HTML_Core.Slot(new Halogen_Component.SlotConstructor(Halogen_Component_ChildPath.injSlot(i)(p), Prelude["<$>"](Prelude.functorFn)(transform)(l)));
            };
        };
    };
};
var slot = function (p) {
    return function (l) {
        return new Halogen_HTML_Core.Slot(new Halogen_Component.SlotConstructor(p, l));
    };
};
module.exports = {
    "slot'": slot$prime, 
    slot: slot, 
    text: text
};

},{"../Halogen.Component":177,"../Halogen.Component.ChildPath":174,"../Halogen.HTML.Core":180,"../Halogen.HTML.Elements":182,"../Prelude":203}],191:[function(require,module,exports){
/* global exports, require */
"use strict";

// module Halogen.Internal.VirtualDOM

var vcreateElement = require("virtual-dom/create-element");
var vdiff = require("virtual-dom/diff");
var vpatch = require("virtual-dom/patch");
var VText = require("virtual-dom/vnode/vtext");
var VirtualNode = require("virtual-dom/vnode/vnode");
var SoftSetHook = require("virtual-dom/virtual-hyperscript/hooks/soft-set-hook");

// jshint maxparams: 2
exports.prop = function (key, value) {
  var props = {};
  props[key] = value;
  return props;
};

// jshint maxparams: 2
exports.attr = function (key, value) {
  var props = { attributes: {} };
  props.attributes[key] = value;
  return props;
};

function HandlerHook (key, f) {
  this.key = key;
  this.callback = function (e) {
    f(e)();
  };
}

HandlerHook.prototype = {
  hook: function (node) {
    node.addEventListener(this.key, this.callback);
  },
  unhook: function (node) {
    node.removeEventListener(this.key, this.callback);
  }
};

// jshint maxparams: 2
exports.handlerProp = function (key, f) {
  var props = {};
  props["halogen-hook-" + key] = new HandlerHook(key, f);
  return props;
};

exports.refPropImpl = function (nothing) {
  return function (just) {

    var ifHookFn = function (init) {
      // jshint maxparams: 3
      return function (node, prop, diff) {
        // jshint validthis: true
        if (typeof diff === "undefined") {
          this.f(init ? just(node) : nothing)();
        }
      };
    };

    // jshint maxparams: 1
    function RefHook (f) {
      this.f = f;
    }

    RefHook.prototype = {
      hook: ifHookFn(true),
      unhook: ifHookFn(false)
    };

    return function (f) {
      return { "halogen-ref": new RefHook(f) };
    };
  };
};

// jshint maxparams: 3
function HalogenWidget (tree, eq, render) {
  this.tree = tree;
  this.eq = eq;
  this.render = render;
  this.vdom = null;
  this.el = null;
}

HalogenWidget.prototype = {
  type: "Widget",
  init: function () {
    this.vdom = this.render(this.tree);
    this.el = vcreateElement(this.vdom);
    return this.el;
  },
  update: function (prev, node) {
    if (!prev.tree || !this.eq(prev.tree.slot)(this.tree.slot)) {
      return this.init();
    }
    if (this.tree.thunk) {
      this.vdom = prev.vdom;
      this.el = prev.el;
    } else {
      this.vdom = this.render(this.tree);
      this.el = vpatch(node, vdiff(prev.vdom, this.vdom));
    }
  }
};

exports.widget = function (tree) {
  return function (eq) {
    return function (render) {
      return new HalogenWidget(tree, eq, render);
    };
  };
};

exports.concatProps = function () {
  // jshint maxparams: 2
  var hOP = Object.prototype.hasOwnProperty;
  var copy = function (props, result) {
    for (var key in props) {
      if (hOP.call(props, key)) {
        if (key === "attributes") {
          var attrs = props[key];
          var resultAttrs = result[key] || (result[key] = {});
          for (var attr in attrs) {
            if (hOP.call(attrs, attr)) {
              resultAttrs[attr] = attrs[attr];
            }
          }
        } else {
          result[key] = props[key];
        }
      }
    }
    return result;
  };
  return function (p1, p2) {
    return copy(p2, copy(p1, {}));
  };
}();

exports.emptyProps = {};

exports.createElement = function (vtree) {
  return vcreateElement(vtree);
};

exports.diff = function (vtree1) {
  return function (vtree2) {
    return vdiff(vtree1, vtree2);
  };
};

exports.patch = function (p) {
  return function (node) {
    return function () {
      return vpatch(node, p);
    };
  };
};

exports.vtext = function (s) {
  return new VText(s);
};

exports.vnode = function (namespace) {
  return function (name) {
    return function (key) {
      return function (props) {
        return function (children) {
          if (name === "input" && props.value !== undefined) {
            props.value = new SoftSetHook(props.value);
          }
          return new VirtualNode(name, props, children, key, namespace);
        };
      };
    };
  };
};

},{"virtual-dom/create-element":3,"virtual-dom/diff":4,"virtual-dom/patch":5,"virtual-dom/virtual-hyperscript/hooks/soft-set-hook":12,"virtual-dom/vnode/vnode":20,"virtual-dom/vnode/vtext":22}],192:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Monoid = require("../Data.Monoid");
var Data_Maybe = require("../Data.Maybe");
var Data_Nullable = require("../Data.Nullable");
var Data_Function = require("../Data.Function");
var DOM = require("../DOM");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var Halogen_Component_Tree = require("../Halogen.Component.Tree");
var semigroupProps = new Prelude.Semigroup(Data_Function.runFn2($foreign.concatProps));
var refProp = $foreign.refPropImpl(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
var monoidProps = new Data_Monoid.Monoid(function () {
    return semigroupProps;
}, $foreign.emptyProps);
module.exports = {
    refProp: refProp, 
    semigroupProps: semigroupProps, 
    monoidProps: monoidProps, 
    vnode: $foreign.vnode, 
    vtext: $foreign.vtext, 
    patch: $foreign.patch, 
    diff: $foreign.diff, 
    createElement: $foreign.createElement, 
    widget: $foreign.widget, 
    handlerProp: $foreign.handlerProp, 
    attr: $foreign.attr, 
    prop: $foreign.prop
};

},{"../Control.Monad.Eff":54,"../DOM":98,"../DOM.HTML.Types":87,"../Data.Function":130,"../Data.Maybe":152,"../Data.Monoid":159,"../Data.Nullable":162,"../Halogen.Component.Tree":176,"../Prelude":203,"./foreign":191}],193:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Bind = require("../Control.Bind");
var Control_Coroutine_Aff = require("../Control.Coroutine.Aff");
var Control_Coroutine_Stalling = require("../Control.Coroutine.Stalling");
var Control_Monad_Aff_AVar = require("../Control.Monad.Aff.AVar");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_Free = require("../Control.Monad.Free");
var Data_Const = require("../Data.Const");
var Data_Either = require("../Data.Either");
var Data_Functor_Coproduct = require("../Data.Functor.Coproduct");
var Data_Maybe = require("../Data.Maybe");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var EventSource = function (x) {
    return x;
};
var toParentEventSource = Unsafe_Coerce.unsafeCoerce;
var runEventSource = function (v) {
    return v;
};
var fromParentEventSource = Unsafe_Coerce.unsafeCoerce;
var eventSource_ = function (dictMonad) {
    return function (dictMonadAff) {
        return function (attach) {
            return function (handle) {
                return EventSource(Control_Coroutine_Stalling.producerToStallingProducer(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Control_Coroutine_Aff["produce'"](dictMonadAff)(function (emit) {
                    return attach(Control_Bind["=<<"](Control_Monad_Eff.bindEff)(function ($9) {
                        return emit(Data_Either.Left.create($9));
                    })(handle));
                })));
            };
        };
    };
};
var eventSource = function (dictMonad) {
    return function (dictMonadAff) {
        return function (attach) {
            return function (handle) {
                return EventSource(Control_Coroutine_Stalling.producerToStallingProducer(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Control_Coroutine_Aff["produce'"](dictMonadAff)(function (emit) {
                    return attach(Control_Bind["<=<"](Control_Monad_Eff.bindEff)(function ($10) {
                        return emit(Data_Either.Left.create($10));
                    })(handle));
                })));
            };
        };
    };
};
var catEventSource = function (dictMonadRec) {
    return function (v) {
        return EventSource(Control_Coroutine_Stalling.catMaybes(dictMonadRec)(Control_Coroutine_Stalling.mapStallingProducer((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Functor_Coproduct.coproduct(Prelude["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create))(v)));
    };
};
module.exports = {
    EventSource: EventSource, 
    fromParentEventSource: fromParentEventSource, 
    toParentEventSource: toParentEventSource, 
    catEventSource: catEventSource, 
    eventSource_: eventSource_, 
    eventSource: eventSource, 
    runEventSource: runEventSource
};

},{"../Control.Bind":31,"../Control.Coroutine.Aff":33,"../Control.Coroutine.Stalling":34,"../Control.Monad.Aff.AVar":39,"../Control.Monad.Aff.Class":40,"../Control.Monad.Eff":54,"../Control.Monad.Free":58,"../Control.Monad.Rec.Class":65,"../Data.Const":110,"../Data.Either":114,"../Data.Functor.Coproduct":132,"../Data.Maybe":152,"../Prelude":203,"../Unsafe.Coerce":206}],194:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Monad_Aff_Free = require("../Control.Monad.Aff.Free");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Control_Plus = require("../Control.Plus");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Maybe = require("../Data.Maybe");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var Halogen_Query_EventSource = require("../Halogen.Query.EventSource");
var Halogen_Query_StateF = require("../Halogen.Query.StateF");
var Control_Coroutine_Stalling = require("../Control.Coroutine.Stalling");
var Pending = (function () {
    function Pending() {

    };
    Pending.value = new Pending();
    return Pending;
})();
var Deferred = (function () {
    function Deferred() {

    };
    Deferred.value = new Deferred();
    return Deferred;
})();
var StateHF = (function () {
    function StateHF(value0) {
        this.value0 = value0;
    };
    StateHF.create = function (value0) {
        return new StateHF(value0);
    };
    return StateHF;
})();
var SubscribeHF = (function () {
    function SubscribeHF(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SubscribeHF.create = function (value0) {
        return function (value1) {
            return new SubscribeHF(value0, value1);
        };
    };
    return SubscribeHF;
})();
var QueryHF = (function () {
    function QueryHF(value0) {
        this.value0 = value0;
    };
    QueryHF.create = function (value0) {
        return new QueryHF(value0);
    };
    return QueryHF;
})();
var RenderHF = (function () {
    function RenderHF(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    RenderHF.create = function (value0) {
        return function (value1) {
            return new RenderHF(value0, value1);
        };
    };
    return RenderHF;
})();
var RenderPendingHF = (function () {
    function RenderPendingHF(value0) {
        this.value0 = value0;
    };
    RenderPendingHF.create = function (value0) {
        return new RenderPendingHF(value0);
    };
    return RenderPendingHF;
})();
var HaltHF = (function () {
    function HaltHF() {

    };
    HaltHF.value = new HaltHF();
    return HaltHF;
})();
var transformHF = function (dictFunctor) {
    return function (sigma) {
        return function (phi) {
            return function (gamma) {
                return function (h) {
                    if (h instanceof StateHF) {
                        return new StateHF(sigma(h.value0));
                    };
                    if (h instanceof SubscribeHF) {
                        return new SubscribeHF(Control_Monad_Free_Trans.bimapFreeT(Control_Coroutine_Stalling.functorStallF)(dictFunctor)(Data_Bifunctor.lmap(Control_Coroutine_Stalling.bifunctorStallF)(phi))(gamma)(Halogen_Query_EventSource.runEventSource(h.value0)), h.value1);
                    };
                    if (h instanceof QueryHF) {
                        return new QueryHF(gamma(h.value0));
                    };
                    if (h instanceof RenderHF) {
                        return new RenderHF(h.value0, h.value1);
                    };
                    if (h instanceof RenderPendingHF) {
                        return new RenderPendingHF(h.value0);
                    };
                    if (h instanceof HaltHF) {
                        return HaltHF.value;
                    };
                    throw new Error("Failed pattern match at Halogen.Query.HalogenF line 65, column 3 - line 74, column 1: " + [ h.constructor.name ]);
                };
            };
        };
    };
};
var hoistHalogenF = function (dictFunctor) {
    return function (eta) {
        return function (h) {
            if (h instanceof StateHF) {
                return new StateHF(h.value0);
            };
            if (h instanceof SubscribeHF) {
                return new SubscribeHF(Control_Monad_Free_Trans.hoistFreeT(Control_Coroutine_Stalling.functorStallF)(dictFunctor)(eta)(Halogen_Query_EventSource.runEventSource(h.value0)), h.value1);
            };
            if (h instanceof QueryHF) {
                return new QueryHF(eta(h.value0));
            };
            if (h instanceof RenderHF) {
                return new RenderHF(h.value0, h.value1);
            };
            if (h instanceof RenderPendingHF) {
                return new RenderPendingHF(h.value0);
            };
            if (h instanceof HaltHF) {
                return HaltHF.value;
            };
            throw new Error("Failed pattern match at Halogen.Query.HalogenF line 80, column 3 - line 86, column 15: " + [ h.constructor.name ]);
        };
    };
};
var functorHalogenF = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        return function (h) {
            if (h instanceof StateHF) {
                return new StateHF(Prelude.map(Halogen_Query_StateF.functorStateF)(f)(h.value0));
            };
            if (h instanceof SubscribeHF) {
                return new SubscribeHF(h.value0, f(h.value1));
            };
            if (h instanceof QueryHF) {
                return new QueryHF(Prelude.map(dictFunctor)(f)(h.value0));
            };
            if (h instanceof RenderHF) {
                return new RenderHF(h.value0, f(h.value1));
            };
            if (h instanceof RenderPendingHF) {
                return new RenderPendingHF(Prelude["<$>"](Prelude.functorFn)(f)(h.value0));
            };
            if (h instanceof HaltHF) {
                return HaltHF.value;
            };
            throw new Error("Failed pattern match at Halogen.Query.HalogenF line 38, column 5 - line 46, column 1: " + [ h.constructor.name ]);
        };
    });
};
var altHalogenF = function (dictFunctor) {
    return new Control_Alt.Alt(function () {
        return functorHalogenF(dictFunctor);
    }, function (v) {
        return function (v1) {
            if (v instanceof HaltHF) {
                return v1;
            };
            return v;
        };
    });
};
var plusHalogenF = function (dictFunctor) {
    return new Control_Plus.Plus(function () {
        return altHalogenF(dictFunctor);
    }, HaltHF.value);
};
var affableHalogenF = function (dictAffable) {
    return new Control_Monad_Aff_Free.Affable(function ($34) {
        return QueryHF.create(Control_Monad_Aff_Free.fromAff(dictAffable)($34));
    });
};
module.exports = {
    Pending: Pending, 
    Deferred: Deferred, 
    StateHF: StateHF, 
    SubscribeHF: SubscribeHF, 
    QueryHF: QueryHF, 
    RenderHF: RenderHF, 
    RenderPendingHF: RenderPendingHF, 
    HaltHF: HaltHF, 
    hoistHalogenF: hoistHalogenF, 
    transformHF: transformHF, 
    functorHalogenF: functorHalogenF, 
    affableHalogenF: affableHalogenF, 
    altHalogenF: altHalogenF, 
    plusHalogenF: plusHalogenF
};

},{"../Control.Alt":26,"../Control.Coroutine.Stalling":34,"../Control.Monad.Aff.Free":41,"../Control.Monad.Free.Trans":57,"../Control.Plus":77,"../Data.Bifunctor":104,"../Data.Maybe":152,"../Data.NaturalTransformation":160,"../Halogen.Query.EventSource":193,"../Halogen.Query.StateF":195,"../Prelude":203}],195:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_State = require("../Control.Monad.State");
var Data_Functor = require("../Data.Functor");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Get = (function () {
    function Get(value0) {
        this.value0 = value0;
    };
    Get.create = function (value0) {
        return new Get(value0);
    };
    return Get;
})();
var Modify = (function () {
    function Modify(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Modify.create = function (value0) {
        return function (value1) {
            return new Modify(value0, value1);
        };
    };
    return Modify;
})();
var stateN = function (dictMonad) {
    return function (dictMonadState) {
        return function (v) {
            if (v instanceof Get) {
                return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(Control_Monad_State_Class.get(dictMonadState))(function ($22) {
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(v.value0($22));
                });
            };
            if (v instanceof Modify) {
                return Data_Functor["$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Control_Monad_State_Class.modify(dictMonadState)(v.value0))(v.value1);
            };
            throw new Error("Failed pattern match at Halogen.Query.StateF line 34, column 1 - line 35, column 1: " + [ v.constructor.name ]);
        };
    };
};
var mapState = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Get) {
                return new Get(function ($23) {
                    return v2.value0(v($23));
                });
            };
            if (v2 instanceof Modify) {
                return new Modify(v1(v2.value0), v2.value1);
            };
            throw new Error("Failed pattern match at Halogen.Query.StateF line 28, column 1 - line 29, column 1: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var functorStateF = new Prelude.Functor(function (f) {
    return function (v) {
        if (v instanceof Get) {
            return new Get(function ($24) {
                return f(v.value0($24));
            });
        };
        if (v instanceof Modify) {
            return new Modify(v.value0, f(v.value1));
        };
        throw new Error("Failed pattern match at Halogen.Query.StateF line 22, column 3 - line 23, column 3: " + [ f.constructor.name, v.constructor.name ]);
    };
});
module.exports = {
    Get: Get, 
    Modify: Modify, 
    stateN: stateN, 
    mapState: mapState, 
    functorStateF: functorStateF
};

},{"../Control.Monad.State":70,"../Control.Monad.State.Class":68,"../Data.Functor":134,"../Data.NaturalTransformation":160,"../Prelude":203}],196:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Aff_Free = require("../Control.Monad.Aff.Free");
var Control_Monad_Free = require("../Control.Monad.Free");
var Halogen_Query_EventSource = require("../Halogen.Query.EventSource");
var Halogen_Query_HalogenF = require("../Halogen.Query.HalogenF");
var Halogen_Query_StateF = require("../Halogen.Query.StateF");
var subscribe$prime = function (es) {
    return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.SubscribeHF(Halogen_Query_EventSource.toParentEventSource(es), Prelude.unit));
};
var subscribe = function (es) {
    return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.SubscribeHF(es, Prelude.unit));
};
var request = function (req) {
    return req(Prelude.id(Prelude.categoryFn));
};
var modify = function (f) {
    return Control_Monad_Free.liftF(new Halogen_Query_HalogenF.StateHF(new Halogen_Query_StateF.Modify(f, Prelude.unit)));
};
var set = function ($0) {
    return modify(Prelude["const"]($0));
};
var liftH = function ($1) {
    return Control_Monad_Free.liftF(Halogen_Query_HalogenF.QueryHF.create($1));
};
var gets = function ($2) {
    return Control_Monad_Free.liftF(Halogen_Query_HalogenF.StateHF.create(Halogen_Query_StateF.Get.create($2)));
};
var get = gets(Prelude.id(Prelude.categoryFn));
var action = function (act) {
    return act(Prelude.unit);
};
module.exports = {
    liftH: liftH, 
    "subscribe'": subscribe$prime, 
    subscribe: subscribe, 
    set: set, 
    modify: modify, 
    gets: gets, 
    get: get, 
    request: request, 
    action: action
};

},{"../Control.Monad.Aff.Free":41,"../Control.Monad.Free":58,"../Halogen.Query.EventSource":193,"../Halogen.Query.HalogenF":194,"../Halogen.Query.StateF":195,"../Prelude":203}],197:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Data_Maybe = require("../Data.Maybe");
var Data_Either = require("../Data.Either");
var Data_Nullable = require("../Data.Nullable");
var Data_Foreign = require("../Data.Foreign");
var DOM = require("../DOM");
var DOM_Event_EventTarget = require("../DOM.Event.EventTarget");
var DOM_Event_EventTypes = require("../DOM.Event.EventTypes");
var DOM_HTML = require("../DOM.HTML");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_HTML_Window = require("../DOM.HTML.Window");
var DOM_Node_ParentNode = require("../DOM.Node.ParentNode");
var Halogen_Effects = require("../Halogen.Effects");
var selectElement = function (query) {
    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Prelude["<$>"](Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)(Control_Bind["=<<"](Control_Monad_Eff.bindEff)(Control_Bind["<=<"](Control_Monad_Eff.bindEff)(function ($8) {
        return DOM_Node_ParentNode.querySelector(query)(DOM_HTML_Types.htmlDocumentToParentNode($8));
    })(DOM_HTML_Window.document))(DOM_HTML.window))))(function (v) {
        return Prelude.pure(Control_Monad_Aff.applicativeAff)((function () {
            if (v instanceof Data_Maybe.Nothing) {
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Data_Maybe.Just) {
                return Data_Either.either(Prelude["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create)(DOM_HTML_Types.readHTMLElement(Data_Foreign.toForeign(v.value0)));
            };
            throw new Error("Failed pattern match at Halogen.Util line 54, column 3 - line 60, column 1: " + [ v.constructor.name ]);
        })());
    });
};
var runHalogenAff = Control_Monad_Aff.runAff(Control_Monad_Eff_Exception.throwException)(Prelude["const"](Prelude.pure(Control_Monad_Eff.applicativeEff)(Prelude.unit)));
var awaitLoad = Control_Monad_Aff.makeAff(function (v) {
    return function (callback) {
        return Control_Monad_Eff_Class.liftEff(Control_Monad_Eff_Class.monadEffEff)(function __do() {
            var $9 = DOM_HTML.window();
            return DOM_Event_EventTarget.addEventListener(DOM_Event_EventTypes.load)(DOM_Event_EventTarget.eventListener(function (v1) {
                return callback(Prelude.unit);
            }))(false)(DOM_HTML_Types.windowToEventTarget($9))();
        });
    };
});
var awaitBody = Prelude.bind(Control_Monad_Aff.bindAff)(awaitLoad)(function () {
    return Control_Bind["=<<"](Control_Monad_Aff.bindAff)(Data_Maybe.maybe(Control_Monad_Error_Class.throwError(Control_Monad_Aff.monadErrorAff)(Control_Monad_Eff_Exception.error("Could not find body")))(Prelude.pure(Control_Monad_Aff.applicativeAff)))(selectElement("body"));
});
module.exports = {
    runHalogenAff: runHalogenAff, 
    selectElement: selectElement, 
    awaitBody: awaitBody, 
    awaitLoad: awaitLoad
};

},{"../Control.Bind":31,"../Control.Monad.Aff":43,"../Control.Monad.Eff":54,"../Control.Monad.Eff.Class":46,"../Control.Monad.Eff.Exception":48,"../Control.Monad.Error.Class":55,"../DOM":98,"../DOM.Event.EventTarget":81,"../DOM.Event.EventTypes":82,"../DOM.HTML":91,"../DOM.HTML.Types":87,"../DOM.HTML.Window":89,"../DOM.Node.ParentNode":96,"../Data.Either":114,"../Data.Foreign":128,"../Data.Maybe":152,"../Data.Nullable":162,"../Halogen.Effects":179,"../Prelude":203}],198:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var Halogen_Component = require("../Halogen.Component");
var Halogen_Driver = require("../Halogen.Driver");
var Halogen_Effects = require("../Halogen.Effects");
var Halogen_Query = require("../Halogen.Query");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
module.exports = {};

},{"../Data.NaturalTransformation":160,"../Halogen.Component":177,"../Halogen.Driver":178,"../Halogen.Effects":179,"../Halogen.HTML.Core":180,"../Halogen.Query":196,"../Prelude":203}],199:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Halogen = require("../Halogen");
var Halogen_Util = require("../Halogen.Util");
var Halogen_HTML_Properties_Indexed_1 = require("../Halogen.HTML.Properties.Indexed");
var Halogen_HTML_Elements = require("../Halogen.HTML.Elements");
var Halogen_HTML_Indexed = require("../Halogen.HTML.Indexed");
var Data_Generic = require("../Data.Generic");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_HTML_Elements_Indexed = require("../Halogen.HTML.Elements.Indexed");
var Halogen_HTML_Properties_Indexed_1 = require("../Halogen.HTML.Properties.Indexed");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Data_Maybe = require("../Data.Maybe");
var Control_Monad_Eff_JQuery = require("../Control.Monad.Eff.JQuery");
var Halogen_HTML = require("../Halogen.HTML");
var Control_Monad_Free = require("../Control.Monad.Free");
var Control_Monad_Aff_Free = require("../Control.Monad.Aff.Free");
var Halogen_Query_HalogenF = require("../Halogen.Query.HalogenF");
var Halogen_Component = require("../Halogen.Component");
var Halogen_Query = require("../Halogen.Query");
var Halogen_Driver = require("../Halogen.Driver");
var Slot = (function () {
    function Slot() {

    };
    Slot.value = new Slot();
    return Slot;
})();
var Initialize = (function () {
    function Initialize(value0) {
        this.value0 = value0;
    };
    Initialize.create = function (value0) {
        return new Initialize(value0);
    };
    return Initialize;
})();
var Finalize = (function () {
    function Finalize(value0) {
        this.value0 = value0;
    };
    Finalize.create = function (value0) {
        return new Finalize(value0);
    };
    return Finalize;
})();
var slotGeneric = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Main.Slot" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Slot.value);
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Main.Slot", [ {
        sigConstructor: "Main.Slot", 
        sigValues: [  ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Main.Slot", [  ]);
});
var refine = Unsafe_Coerce.unsafeCoerce;
var onOffProp = function (pName) {
    return function ($17) {
        return Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName(pName))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName(pName)))((function (b) {
            if (b) {
                return "on";
            };
            if (!b) {
                return "off";
            };
            throw new Error("Failed pattern match at Main line 26, column 76 - line 26, column 101: " + [ b.constructor.name ]);
        })($17));
    };
};
var inputx = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.input);
var eqSlot = new Prelude.Eq(Data_Generic.gEq(slotGeneric));
var ordGeneric = new Prelude.Ord(function () {
    return eqSlot;
}, Data_Generic.gCompare(slotGeneric));
var autoCorrectP = onOffProp("autocorrect");
var autocorrect = refine(autoCorrectP);
var autoCapitalizeP = onOffProp("autocapitalize");
var autocapitalize = refine(autoCapitalizeP);
var ui = (function () {
    var render = function (st) {
        return Halogen_HTML_Elements.div_([ Halogen_HTML_Elements.div_([ Halogen_HTML.text("autocorrect attribute not added") ]), inputx([ Halogen_HTML_Properties_Indexed_1.name("example"), Halogen_HTML_Properties_Indexed_1.autocomplete(false), autocorrect(false), autocapitalize(false), Halogen_HTML_Properties_Indexed_1.spellcheck(false), Halogen_HTML_Properties_Indexed_1.inputType(Halogen_HTML_Properties_Indexed_1.InputText.value) ]), Halogen_HTML_Elements.div_([ Halogen_HTML.text("autocorrect attribute added through jquery") ]), inputx([ Halogen_HTML_Properties_Indexed_1.name("working-example"), Halogen_HTML_Properties_Indexed_1.id_("working-example"), Halogen_HTML_Properties_Indexed_1.autocomplete(false), autocorrect(false), autocapitalize(false), Halogen_HTML_Properties_Indexed_1.spellcheck(false), Halogen_HTML_Properties_Indexed_1.inputType(Halogen_HTML_Properties_Indexed_1.InputText.value) ]) ]);
    };
    var $$eval = function (v) {
        if (v instanceof Initialize) {
            return Prelude.bind(Control_Monad_Free.freeBind)(Control_Monad_Aff_Free.fromEff(Control_Monad_Aff_Free.affableFree(Halogen_Query_HalogenF.affableHalogenF(Control_Monad_Aff_Free.affableAff)))(Control_Monad_Eff_JQuery.select("#working-example")))(function (v1) {
                return Prelude.bind(Control_Monad_Free.freeBind)(Control_Monad_Aff_Free.fromEff(Control_Monad_Aff_Free.affableFree(Halogen_Query_HalogenF.affableHalogenF(Control_Monad_Aff_Free.affableAff)))(Control_Monad_Eff_JQuery.setAttr("autocorrect")("off")(v1)))(function (v2) {
                    return Prelude.pure(Control_Monad_Free.freeApplicative)(v.value0);
                });
            });
        };
        if (v instanceof Finalize) {
            return Prelude.pure(Control_Monad_Free.freeApplicative)(v.value0);
        };
        throw new Error("Failed pattern match at Main line 82, column 3 - line 86, column 3: " + [ v.constructor.name ]);
    };
    return Halogen_Component.lifecycleComponent({
        render: render, 
        "eval": $$eval, 
        initializer: new Data_Maybe.Just(Halogen_Query.action(Initialize.create)), 
        finalizer: new Data_Maybe.Just(Halogen_Query.action(Finalize.create))
    });
})();
var main = Halogen_Util.runHalogenAff(Prelude.bind(Control_Monad_Aff.bindAff)(Halogen_Util.awaitBody)(function (v) {
    return Halogen_Driver.runUI(ui)({})(v);
}));
module.exports = {
    Slot: Slot, 
    Initialize: Initialize, 
    Finalize: Finalize, 
    main: main, 
    ui: ui, 
    autocorrect: autocorrect, 
    autoCorrectP: autoCorrectP, 
    autocapitalize: autocapitalize, 
    autoCapitalizeP: autoCapitalizeP, 
    onOffProp: onOffProp, 
    refine: refine, 
    inputx: inputx, 
    slotGeneric: slotGeneric, 
    eqSlot: eqSlot, 
    ordGeneric: ordGeneric
};

},{"../Control.Monad.Aff":43,"../Control.Monad.Aff.Free":41,"../Control.Monad.Eff":54,"../Control.Monad.Eff.JQuery":50,"../Control.Monad.Free":58,"../Data.Generic":136,"../Data.Maybe":152,"../Halogen":198,"../Halogen.Component":177,"../Halogen.Driver":178,"../Halogen.HTML":190,"../Halogen.HTML.Core":180,"../Halogen.HTML.Elements":182,"../Halogen.HTML.Elements.Indexed":181,"../Halogen.HTML.Indexed":186,"../Halogen.HTML.Properties.Indexed":187,"../Halogen.Query":196,"../Halogen.Query.HalogenF":194,"../Halogen.Util":197,"../Prelude":203,"../Unsafe.Coerce":206}],200:[function(require,module,exports){
/* global exports */
"use strict";

// module Math

exports.abs = Math.abs;

exports.acos = Math.acos;

exports.asin = Math.asin;

exports.atan = Math.atan;

exports.atan2 = function (y) {
  return function (x) {
    return Math.atan2(y, x);
  };
};

exports.ceil = Math.ceil;

exports.cos = Math.cos;

exports.exp = Math.exp;

exports.floor = Math.floor;

exports.log = Math.log;

exports.max = function (n1) {
  return function (n2) {
    return Math.max(n1, n2);
  };
};

exports.min = function (n1) {
  return function (n2) {
    return Math.min(n1, n2);
  };
};

exports.pow = function (n) {
  return function (p) {
    return Math.pow(n, p);
  };
};

exports["%"] = function(n) {
  return function(m) {
    return n % m;
  };
};

exports.round = Math.round;

exports.sin = Math.sin;

exports.sqrt = Math.sqrt;

exports.tan = Math.tan;

exports.e = Math.E;

exports.ln2 = Math.LN2;

exports.ln10 = Math.LN10;

exports.log2e = Math.LOG2E;

exports.log10e = Math.LOG10E;

exports.pi = Math.PI;

exports.sqrt1_2 = Math.SQRT1_2;

exports.sqrt2 = Math.SQRT2;

},{}],201:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
module.exports = {
    sqrt2: $foreign.sqrt2, 
    sqrt1_2: $foreign.sqrt1_2, 
    pi: $foreign.pi, 
    log10e: $foreign.log10e, 
    log2e: $foreign.log2e, 
    ln10: $foreign.ln10, 
    ln2: $foreign.ln2, 
    e: $foreign.e, 
    "%": $foreign["%"], 
    tan: $foreign.tan, 
    sqrt: $foreign.sqrt, 
    sin: $foreign.sin, 
    round: $foreign.round, 
    pow: $foreign.pow, 
    min: $foreign.min, 
    max: $foreign.max, 
    log: $foreign.log, 
    floor: $foreign.floor, 
    exp: $foreign.exp, 
    cos: $foreign.cos, 
    ceil: $foreign.ceil, 
    atan2: $foreign.atan2, 
    atan: $foreign.atan, 
    asin: $foreign.asin, 
    acos: $foreign.acos, 
    abs: $foreign.abs
};

},{"./foreign":200}],202:[function(require,module,exports){
/* global exports */
"use strict";

// module Prelude

//- Functor --------------------------------------------------------------------

exports.arrayMap = function (f) {
  return function (arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
};

//- Bind -----------------------------------------------------------------------

exports.arrayBind = function (arr) {
  return function (f) {
    var result = [];
    for (var i = 0, l = arr.length; i < l; i++) {
      Array.prototype.push.apply(result, f(arr[i]));
    }
    return result;
  };
};

//- Monoid ---------------------------------------------------------------------

exports.concatString = function (s1) {
  return function (s2) {
    return s1 + s2;
  };
};

exports.concatArray = function (xs) {
  return function (ys) {
    return xs.concat(ys);
  };
};

//- Semiring -------------------------------------------------------------------

exports.intAdd = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x + y | 0;
  };
};

exports.intMul = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x * y | 0;
  };
};

exports.numAdd = function (n1) {
  return function (n2) {
    return n1 + n2;
  };
};

exports.numMul = function (n1) {
  return function (n2) {
    return n1 * n2;
  };
};

//- ModuloSemiring -------------------------------------------------------------

exports.intDiv = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x / y | 0;
  };
};

exports.intMod = function (x) {
  return function (y) {
    return x % y;
  };
};

exports.numDiv = function (n1) {
  return function (n2) {
    return n1 / n2;
  };
};

//- Ring -----------------------------------------------------------------------

exports.intSub = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x - y | 0;
  };
};

exports.numSub = function (n1) {
  return function (n2) {
    return n1 - n2;
  };
};

//- Eq -------------------------------------------------------------------------

exports.refEq = function (r1) {
  return function (r2) {
    return r1 === r2;
  };
};

exports.refIneq = function (r1) {
  return function (r2) {
    return r1 !== r2;
  };
};

exports.eqArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      if (xs.length !== ys.length) return false;
      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }
      return true;
    };
  };
};

exports.ordArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      var i = 0;
      var xlen = xs.length;
      var ylen = ys.length;
      while (i < xlen && i < ylen) {
        var x = xs[i];
        var y = ys[i];
        var o = f(x)(y);
        if (o !== 0) {
          return o;
        }
        i++;
      }
      if (xlen === ylen) {
        return 0;
      } else if (xlen > ylen) {
        return -1;
      } else {
        return 1;
      }
    };
  };
};

//- Ord ------------------------------------------------------------------------

exports.unsafeCompareImpl = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (x) {
        return function (y) {
          return x < y ? lt : x > y ? gt : eq;
        };
      };
    };
  };
};

//- Bounded --------------------------------------------------------------------

exports.topInt = 2147483647;
exports.bottomInt = -2147483648;

exports.topChar = String.fromCharCode(65535);
exports.bottomChar = String.fromCharCode(0);

//- BooleanAlgebra -------------------------------------------------------------

exports.boolOr = function (b1) {
  return function (b2) {
    return b1 || b2;
  };
};

exports.boolAnd = function (b1) {
  return function (b2) {
    return b1 && b2;
  };
};

exports.boolNot = function (b) {
  return !b;
};

//- Show -----------------------------------------------------------------------

exports.showIntImpl = function (n) {
  return n.toString();
};

exports.showNumberImpl = function (n) {
  /* jshint bitwise: false */
  return n === (n | 0) ? n + ".0" : n.toString();
};

exports.showCharImpl = function (c) {
  var code = c.charCodeAt(0);
  if (code < 0x20 || code === 0x7F) {
    switch (c) {
      case "\x07": return "'\\a'";
      case "\b": return "'\\b'";
      case "\f": return "'\\f'";
      case "\n": return "'\\n'";
      case "\r": return "'\\r'";
      case "\t": return "'\\t'";
      case "\v": return "'\\v'";
    }
    return "'\\" + code.toString(10) + "'";
  }
  return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
};

exports.showStringImpl = function (s) {
  var l = s.length;
  return "\"" + s.replace(
    /[\0-\x1F\x7F"\\]/g,
    function (c, i) { // jshint ignore:line
      switch (c) {
        case "\"":
        case "\\":
          return "\\" + c;
        case "\x07": return "\\a";
        case "\b": return "\\b";
        case "\f": return "\\f";
        case "\n": return "\\n";
        case "\r": return "\\r";
        case "\t": return "\\t";
        case "\v": return "\\v";
      }
      var k = i + 1;
      var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty;
    }
  ) + "\"";
};

exports.showArrayImpl = function (f) {
  return function (xs) {
    var ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
};

},{}],203:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
var Unit = function (x) {
    return x;
};
var LT = (function () {
    function LT() {

    };
    LT.value = new LT();
    return LT;
})();
var GT = (function () {
    function GT() {

    };
    GT.value = new GT();
    return GT;
})();
var EQ = (function () {
    function EQ() {

    };
    EQ.value = new EQ();
    return EQ;
})();
var Semigroupoid = function (compose) {
    this.compose = compose;
};
var Category = function (__superclass_Prelude$dotSemigroupoid_0, id) {
    this["__superclass_Prelude.Semigroupoid_0"] = __superclass_Prelude$dotSemigroupoid_0;
    this.id = id;
};
var Functor = function (map) {
    this.map = map;
};
var Apply = function (__superclass_Prelude$dotFunctor_0, apply) {
    this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    this.apply = apply;
};
var Applicative = function (__superclass_Prelude$dotApply_0, pure) {
    this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
    this.pure = pure;
};
var Bind = function (__superclass_Prelude$dotApply_0, bind) {
    this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
    this.bind = bind;
};
var Monad = function (__superclass_Prelude$dotApplicative_0, __superclass_Prelude$dotBind_1) {
    this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
    this["__superclass_Prelude.Bind_1"] = __superclass_Prelude$dotBind_1;
};
var Semigroup = function (append) {
    this.append = append;
};
var Semiring = function (add, mul, one, zero) {
    this.add = add;
    this.mul = mul;
    this.one = one;
    this.zero = zero;
};
var Ring = function (__superclass_Prelude$dotSemiring_0, sub) {
    this["__superclass_Prelude.Semiring_0"] = __superclass_Prelude$dotSemiring_0;
    this.sub = sub;
};
var ModuloSemiring = function (__superclass_Prelude$dotSemiring_0, div, mod) {
    this["__superclass_Prelude.Semiring_0"] = __superclass_Prelude$dotSemiring_0;
    this.div = div;
    this.mod = mod;
};
var DivisionRing = function (__superclass_Prelude$dotModuloSemiring_1, __superclass_Prelude$dotRing_0) {
    this["__superclass_Prelude.ModuloSemiring_1"] = __superclass_Prelude$dotModuloSemiring_1;
    this["__superclass_Prelude.Ring_0"] = __superclass_Prelude$dotRing_0;
};
var Num = function (__superclass_Prelude$dotDivisionRing_0) {
    this["__superclass_Prelude.DivisionRing_0"] = __superclass_Prelude$dotDivisionRing_0;
};
var Eq = function (eq) {
    this.eq = eq;
};
var Ord = function (__superclass_Prelude$dotEq_0, compare) {
    this["__superclass_Prelude.Eq_0"] = __superclass_Prelude$dotEq_0;
    this.compare = compare;
};
var Bounded = function (bottom, top) {
    this.bottom = bottom;
    this.top = top;
};
var BoundedOrd = function (__superclass_Prelude$dotBounded_0, __superclass_Prelude$dotOrd_1) {
    this["__superclass_Prelude.Bounded_0"] = __superclass_Prelude$dotBounded_0;
    this["__superclass_Prelude.Ord_1"] = __superclass_Prelude$dotOrd_1;
};
var BooleanAlgebra = function (__superclass_Prelude$dotBounded_0, conj, disj, not) {
    this["__superclass_Prelude.Bounded_0"] = __superclass_Prelude$dotBounded_0;
    this.conj = conj;
    this.disj = disj;
    this.not = not;
};
var Show = function (show) {
    this.show = show;
};
var $dollar = function (f) {
    return function (x) {
        return f(x);
    };
};
var $hash = function (x) {
    return function (f) {
        return f(x);
    };
};
var zero = function (dict) {
    return dict.zero;
};
var unsafeCompare = $foreign.unsafeCompareImpl(LT.value)(EQ.value)(GT.value);
var unit = {};
var top = function (dict) {
    return dict.top;
};
var sub = function (dict) {
    return dict.sub;
};
var $minus = function (dictRing) {
    return sub(dictRing);
};
var showUnit = new Show(function (v) {
    return "unit";
});
var showString = new Show($foreign.showStringImpl);
var showOrdering = new Show(function (v) {
    if (v instanceof LT) {
        return "LT";
    };
    if (v instanceof GT) {
        return "GT";
    };
    if (v instanceof EQ) {
        return "EQ";
    };
    throw new Error("Failed pattern match at Prelude line 864, column 3 - line 865, column 3: " + [ v.constructor.name ]);
});
var showNumber = new Show($foreign.showNumberImpl);
var showInt = new Show($foreign.showIntImpl);
var showChar = new Show($foreign.showCharImpl);
var showBoolean = new Show(function (v) {
    if (v) {
        return "true";
    };
    if (!v) {
        return "false";
    };
    throw new Error("Failed pattern match at Prelude line 842, column 3 - line 843, column 3: " + [ v.constructor.name ]);
});
var show = function (dict) {
    return dict.show;
};
var showArray = function (dictShow) {
    return new Show($foreign.showArrayImpl(show(dictShow)));
};
var semiringUnit = new Semiring(function (v) {
    return function (v1) {
        return unit;
    };
}, function (v) {
    return function (v1) {
        return unit;
    };
}, unit, unit);
var semiringNumber = new Semiring($foreign.numAdd, $foreign.numMul, 1.0, 0.0);
var semiringInt = new Semiring($foreign.intAdd, $foreign.intMul, 1, 0);
var semigroupoidFn = new Semigroupoid(function (f) {
    return function (g) {
        return function (x) {
            return f(g(x));
        };
    };
});
var semigroupUnit = new Semigroup(function (v) {
    return function (v1) {
        return unit;
    };
});
var semigroupString = new Semigroup($foreign.concatString);
var semigroupOrdering = new Semigroup(function (v) {
    return function (v1) {
        if (v instanceof LT) {
            return LT.value;
        };
        if (v instanceof GT) {
            return GT.value;
        };
        if (v instanceof EQ) {
            return v1;
        };
        throw new Error("Failed pattern match at Prelude line 414, column 3 - line 415, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var semigroupArray = new Semigroup($foreign.concatArray);
var ringUnit = new Ring(function () {
    return semiringUnit;
}, function (v) {
    return function (v1) {
        return unit;
    };
});
var ringNumber = new Ring(function () {
    return semiringNumber;
}, $foreign.numSub);
var ringInt = new Ring(function () {
    return semiringInt;
}, $foreign.intSub);
var pure = function (dict) {
    return dict.pure;
};
var $$return = function (dictApplicative) {
    return pure(dictApplicative);
};
var otherwise = true;
var one = function (dict) {
    return dict.one;
};
var not = function (dict) {
    return dict.not;
};
var negate = function (dictRing) {
    return function (a) {
        return $minus(dictRing)(zero(dictRing["__superclass_Prelude.Semiring_0"]()))(a);
    };
};
var mul = function (dict) {
    return dict.mul;
};
var $times = function (dictSemiring) {
    return mul(dictSemiring);
};
var moduloSemiringUnit = new ModuloSemiring(function () {
    return semiringUnit;
}, function (v) {
    return function (v1) {
        return unit;
    };
}, function (v) {
    return function (v1) {
        return unit;
    };
});
var moduloSemiringNumber = new ModuloSemiring(function () {
    return semiringNumber;
}, $foreign.numDiv, function (v) {
    return function (v1) {
        return 0.0;
    };
});
var moduloSemiringInt = new ModuloSemiring(function () {
    return semiringInt;
}, $foreign.intDiv, $foreign.intMod);
var mod = function (dict) {
    return dict.mod;
};
var map = function (dict) {
    return dict.map;
};
var $less$dollar$greater = function (dictFunctor) {
    return map(dictFunctor);
};
var $less$hash$greater = function (dictFunctor) {
    return function (fa) {
        return function (f) {
            return $less$dollar$greater(dictFunctor)(f)(fa);
        };
    };
};
var id = function (dict) {
    return dict.id;
};
var functorArray = new Functor($foreign.arrayMap);
var flip = function (f) {
    return function (b) {
        return function (a) {
            return f(a)(b);
        };
    };
};
var eqUnit = new Eq(function (v) {
    return function (v1) {
        return true;
    };
});
var ordUnit = new Ord(function () {
    return eqUnit;
}, function (v) {
    return function (v1) {
        return EQ.value;
    };
});
var eqString = new Eq($foreign.refEq);
var ordString = new Ord(function () {
    return eqString;
}, unsafeCompare);
var eqOrdering = new Eq(function (v) {
    return function (v1) {
        if (v instanceof LT && v1 instanceof LT) {
            return true;
        };
        if (v instanceof GT && v1 instanceof GT) {
            return true;
        };
        if (v instanceof EQ && v1 instanceof EQ) {
            return true;
        };
        return false;
    };
});
var ordOrdering = new Ord(function () {
    return eqOrdering;
}, function (v) {
    return function (v1) {
        if (v instanceof LT && v1 instanceof LT) {
            return EQ.value;
        };
        if (v instanceof EQ && v1 instanceof EQ) {
            return EQ.value;
        };
        if (v instanceof GT && v1 instanceof GT) {
            return EQ.value;
        };
        if (v instanceof LT) {
            return LT.value;
        };
        if (v instanceof EQ && v1 instanceof LT) {
            return GT.value;
        };
        if (v instanceof EQ && v1 instanceof GT) {
            return LT.value;
        };
        if (v instanceof GT) {
            return GT.value;
        };
        throw new Error("Failed pattern match at Prelude line 669, column 3 - line 670, column 3: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var eqNumber = new Eq($foreign.refEq);
var ordNumber = new Ord(function () {
    return eqNumber;
}, unsafeCompare);
var eqInt = new Eq($foreign.refEq);
var ordInt = new Ord(function () {
    return eqInt;
}, unsafeCompare);
var eqChar = new Eq($foreign.refEq);
var ordChar = new Ord(function () {
    return eqChar;
}, unsafeCompare);
var eqBoolean = new Eq($foreign.refEq);
var ordBoolean = new Ord(function () {
    return eqBoolean;
}, unsafeCompare);
var eq = function (dict) {
    return dict.eq;
};
var $eq$eq = function (dictEq) {
    return eq(dictEq);
};
var eqArray = function (dictEq) {
    return new Eq($foreign.eqArrayImpl($eq$eq(dictEq)));
};
var divisionRingUnit = new DivisionRing(function () {
    return moduloSemiringUnit;
}, function () {
    return ringUnit;
});
var numUnit = new Num(function () {
    return divisionRingUnit;
});
var divisionRingNumber = new DivisionRing(function () {
    return moduloSemiringNumber;
}, function () {
    return ringNumber;
});
var numNumber = new Num(function () {
    return divisionRingNumber;
});
var div = function (dict) {
    return dict.div;
};
var $div = function (dictModuloSemiring) {
    return div(dictModuloSemiring);
};
var disj = function (dict) {
    return dict.disj;
};
var $bar$bar = function (dictBooleanAlgebra) {
    return disj(dictBooleanAlgebra);
};
var $$const = function (a) {
    return function (v) {
        return a;
    };
};
var $$void = function (dictFunctor) {
    return function (fa) {
        return $less$dollar$greater(dictFunctor)($$const(unit))(fa);
    };
};
var conj = function (dict) {
    return dict.conj;
};
var $amp$amp = function (dictBooleanAlgebra) {
    return conj(dictBooleanAlgebra);
};
var compose = function (dict) {
    return dict.compose;
};
var functorFn = new Functor(compose(semigroupoidFn));
var $less$less$less = function (dictSemigroupoid) {
    return compose(dictSemigroupoid);
};
var $greater$greater$greater = function (dictSemigroupoid) {
    return flip(compose(dictSemigroupoid));
};
var compare = function (dict) {
    return dict.compare;
};
var ordArray = function (dictOrd) {
    return new Ord(function () {
        return eqArray(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (xs) {
        return function (ys) {
            return $dollar(compare(ordInt)(0))($foreign.ordArrayImpl(function (x) {
                return function (y) {
                    var $79 = compare(dictOrd)(x)(y);
                    if ($79 instanceof EQ) {
                        return 0;
                    };
                    if ($79 instanceof LT) {
                        return 1;
                    };
                    if ($79 instanceof GT) {
                        return negate(ringInt)(1);
                    };
                    throw new Error("Failed pattern match at Prelude line 661, column 53 - line 664, column 57: " + [ $79.constructor.name ]);
                };
            })(xs)(ys));
        };
    });
};
var $less = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $80 = compare(dictOrd)(a1)(a2);
            if ($80 instanceof LT) {
                return true;
            };
            return false;
        };
    };
};
var $less$eq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $81 = compare(dictOrd)(a1)(a2);
            if ($81 instanceof GT) {
                return false;
            };
            return true;
        };
    };
};
var $greater = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $82 = compare(dictOrd)(a1)(a2);
            if ($82 instanceof GT) {
                return true;
            };
            return false;
        };
    };
};
var $greater$eq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $83 = compare(dictOrd)(a1)(a2);
            if ($83 instanceof LT) {
                return false;
            };
            return true;
        };
    };
};
var categoryFn = new Category(function () {
    return semigroupoidFn;
}, function (x) {
    return x;
});
var boundedUnit = new Bounded(unit, unit);
var boundedOrdering = new Bounded(LT.value, GT.value);
var boundedOrdUnit = new BoundedOrd(function () {
    return boundedUnit;
}, function () {
    return ordUnit;
});
var boundedOrdOrdering = new BoundedOrd(function () {
    return boundedOrdering;
}, function () {
    return ordOrdering;
});
var boundedInt = new Bounded($foreign.bottomInt, $foreign.topInt);
var boundedOrdInt = new BoundedOrd(function () {
    return boundedInt;
}, function () {
    return ordInt;
});
var boundedChar = new Bounded($foreign.bottomChar, $foreign.topChar);
var boundedOrdChar = new BoundedOrd(function () {
    return boundedChar;
}, function () {
    return ordChar;
});
var boundedBoolean = new Bounded(false, true);
var boundedOrdBoolean = new BoundedOrd(function () {
    return boundedBoolean;
}, function () {
    return ordBoolean;
});
var bottom = function (dict) {
    return dict.bottom;
};
var boundedFn = function (dictBounded) {
    return new Bounded(function (v) {
        return bottom(dictBounded);
    }, function (v) {
        return top(dictBounded);
    });
};
var booleanAlgebraUnit = new BooleanAlgebra(function () {
    return boundedUnit;
}, function (v) {
    return function (v1) {
        return unit;
    };
}, function (v) {
    return function (v1) {
        return unit;
    };
}, function (v) {
    return unit;
});
var booleanAlgebraFn = function (dictBooleanAlgebra) {
    return new BooleanAlgebra(function () {
        return boundedFn(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]());
    }, function (fx) {
        return function (fy) {
            return function (a) {
                return conj(dictBooleanAlgebra)(fx(a))(fy(a));
            };
        };
    }, function (fx) {
        return function (fy) {
            return function (a) {
                return disj(dictBooleanAlgebra)(fx(a))(fy(a));
            };
        };
    }, function (fx) {
        return function (a) {
            return not(dictBooleanAlgebra)(fx(a));
        };
    });
};
var booleanAlgebraBoolean = new BooleanAlgebra(function () {
    return boundedBoolean;
}, $foreign.boolAnd, $foreign.boolOr, $foreign.boolNot);
var $div$eq = function (dictEq) {
    return function (x) {
        return function (y) {
            return not(booleanAlgebraBoolean)($eq$eq(dictEq)(x)(y));
        };
    };
};
var bind = function (dict) {
    return dict.bind;
};
var liftM1 = function (dictMonad) {
    return function (f) {
        return function (a) {
            return bind(dictMonad["__superclass_Prelude.Bind_1"]())(a)(function (v) {
                return $$return(dictMonad["__superclass_Prelude.Applicative_0"]())(f(v));
            });
        };
    };
};
var $greater$greater$eq = function (dictBind) {
    return bind(dictBind);
};
var asTypeOf = function (x) {
    return function (v) {
        return x;
    };
};
var applyFn = new Apply(function () {
    return functorFn;
}, function (f) {
    return function (g) {
        return function (x) {
            return f(x)(g(x));
        };
    };
});
var bindFn = new Bind(function () {
    return applyFn;
}, function (m) {
    return function (f) {
        return function (x) {
            return f(m(x))(x);
        };
    };
});
var apply = function (dict) {
    return dict.apply;
};
var $less$times$greater = function (dictApply) {
    return apply(dictApply);
};
var liftA1 = function (dictApplicative) {
    return function (f) {
        return function (a) {
            return $less$times$greater(dictApplicative["__superclass_Prelude.Apply_0"]())(pure(dictApplicative)(f))(a);
        };
    };
};
var applicativeFn = new Applicative(function () {
    return applyFn;
}, $$const);
var monadFn = new Monad(function () {
    return applicativeFn;
}, function () {
    return bindFn;
});
var append = function (dict) {
    return dict.append;
};
var $plus$plus = function (dictSemigroup) {
    return append(dictSemigroup);
};
var $less$greater = function (dictSemigroup) {
    return append(dictSemigroup);
};
var semigroupFn = function (dictSemigroup) {
    return new Semigroup(function (f) {
        return function (g) {
            return function (x) {
                return $less$greater(dictSemigroup)(f(x))(g(x));
            };
        };
    });
};
var ap = function (dictMonad) {
    return function (f) {
        return function (a) {
            return bind(dictMonad["__superclass_Prelude.Bind_1"]())(f)(function (v) {
                return bind(dictMonad["__superclass_Prelude.Bind_1"]())(a)(function (v1) {
                    return $$return(dictMonad["__superclass_Prelude.Applicative_0"]())(v(v1));
                });
            });
        };
    };
};
var monadArray = new Monad(function () {
    return applicativeArray;
}, function () {
    return bindArray;
});
var bindArray = new Bind(function () {
    return applyArray;
}, $foreign.arrayBind);
var applyArray = new Apply(function () {
    return functorArray;
}, ap(monadArray));
var applicativeArray = new Applicative(function () {
    return applyArray;
}, function (x) {
    return [ x ];
});
var add = function (dict) {
    return dict.add;
};
var $plus = function (dictSemiring) {
    return add(dictSemiring);
};
module.exports = {
    LT: LT, 
    GT: GT, 
    EQ: EQ, 
    Show: Show, 
    BooleanAlgebra: BooleanAlgebra, 
    BoundedOrd: BoundedOrd, 
    Bounded: Bounded, 
    Ord: Ord, 
    Eq: Eq, 
    DivisionRing: DivisionRing, 
    Num: Num, 
    Ring: Ring, 
    ModuloSemiring: ModuloSemiring, 
    Semiring: Semiring, 
    Semigroup: Semigroup, 
    Monad: Monad, 
    Bind: Bind, 
    Applicative: Applicative, 
    Apply: Apply, 
    Functor: Functor, 
    Category: Category, 
    Semigroupoid: Semigroupoid, 
    show: show, 
    "||": $bar$bar, 
    "&&": $amp$amp, 
    not: not, 
    disj: disj, 
    conj: conj, 
    bottom: bottom, 
    top: top, 
    unsafeCompare: unsafeCompare, 
    ">=": $greater$eq, 
    "<=": $less$eq, 
    ">": $greater, 
    "<": $less, 
    compare: compare, 
    "/=": $div$eq, 
    "==": $eq$eq, 
    eq: eq, 
    "-": $minus, 
    negate: negate, 
    sub: sub, 
    "/": $div, 
    mod: mod, 
    div: div, 
    "*": $times, 
    "+": $plus, 
    one: one, 
    mul: mul, 
    zero: zero, 
    add: add, 
    "++": $plus$plus, 
    "<>": $less$greater, 
    append: append, 
    ap: ap, 
    liftM1: liftM1, 
    "return": $$return, 
    ">>=": $greater$greater$eq, 
    bind: bind, 
    liftA1: liftA1, 
    pure: pure, 
    "<*>": $less$times$greater, 
    apply: apply, 
    "void": $$void, 
    "<#>": $less$hash$greater, 
    "<$>": $less$dollar$greater, 
    map: map, 
    id: id, 
    ">>>": $greater$greater$greater, 
    "<<<": $less$less$less, 
    compose: compose, 
    otherwise: otherwise, 
    asTypeOf: asTypeOf, 
    "const": $$const, 
    flip: flip, 
    "#": $hash, 
    "$": $dollar, 
    unit: unit, 
    semigroupoidFn: semigroupoidFn, 
    categoryFn: categoryFn, 
    functorFn: functorFn, 
    functorArray: functorArray, 
    applyFn: applyFn, 
    applyArray: applyArray, 
    applicativeFn: applicativeFn, 
    applicativeArray: applicativeArray, 
    bindFn: bindFn, 
    bindArray: bindArray, 
    monadFn: monadFn, 
    monadArray: monadArray, 
    semigroupString: semigroupString, 
    semigroupUnit: semigroupUnit, 
    semigroupFn: semigroupFn, 
    semigroupOrdering: semigroupOrdering, 
    semigroupArray: semigroupArray, 
    semiringInt: semiringInt, 
    semiringNumber: semiringNumber, 
    semiringUnit: semiringUnit, 
    ringInt: ringInt, 
    ringNumber: ringNumber, 
    ringUnit: ringUnit, 
    moduloSemiringInt: moduloSemiringInt, 
    moduloSemiringNumber: moduloSemiringNumber, 
    moduloSemiringUnit: moduloSemiringUnit, 
    divisionRingNumber: divisionRingNumber, 
    divisionRingUnit: divisionRingUnit, 
    numNumber: numNumber, 
    numUnit: numUnit, 
    eqBoolean: eqBoolean, 
    eqInt: eqInt, 
    eqNumber: eqNumber, 
    eqChar: eqChar, 
    eqString: eqString, 
    eqUnit: eqUnit, 
    eqArray: eqArray, 
    eqOrdering: eqOrdering, 
    ordBoolean: ordBoolean, 
    ordInt: ordInt, 
    ordNumber: ordNumber, 
    ordString: ordString, 
    ordChar: ordChar, 
    ordUnit: ordUnit, 
    ordArray: ordArray, 
    ordOrdering: ordOrdering, 
    boundedBoolean: boundedBoolean, 
    boundedUnit: boundedUnit, 
    boundedOrdering: boundedOrdering, 
    boundedInt: boundedInt, 
    boundedChar: boundedChar, 
    boundedFn: boundedFn, 
    boundedOrdBoolean: boundedOrdBoolean, 
    boundedOrdUnit: boundedOrdUnit, 
    boundedOrdOrdering: boundedOrdOrdering, 
    boundedOrdInt: boundedOrdInt, 
    boundedOrdChar: boundedOrdChar, 
    booleanAlgebraBoolean: booleanAlgebraBoolean, 
    booleanAlgebraUnit: booleanAlgebraUnit, 
    booleanAlgebraFn: booleanAlgebraFn, 
    showBoolean: showBoolean, 
    showInt: showInt, 
    showNumber: showNumber, 
    showChar: showChar, 
    showString: showString, 
    showUnit: showUnit, 
    showArray: showArray, 
    showOrdering: showOrdering
};

},{"./foreign":202}],204:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var Proxy3 = (function () {
    function Proxy3() {

    };
    Proxy3.value = new Proxy3();
    return Proxy3;
})();
var Proxy2 = (function () {
    function Proxy2() {

    };
    Proxy2.value = new Proxy2();
    return Proxy2;
})();
var $$Proxy = (function () {
    function Proxy() {

    };
    Proxy.value = new Proxy();
    return Proxy;
})();
module.exports = {
    Proxy3: Proxy3, 
    Proxy2: Proxy2, 
    "Proxy": $$Proxy
};

},{}],205:[function(require,module,exports){
"use strict";

// module Unsafe.Coerce

exports.unsafeCoerce = function(x) { return x; }

},{}],206:[function(require,module,exports){
// Generated by psc version 0.8.5.0
"use strict";
var $foreign = require("./foreign");
module.exports = {
    unsafeCoerce: $foreign.unsafeCoerce
};

},{"./foreign":205}],207:[function(require,module,exports){
require('Main').main();

},{"Main":199}],208:[function(require,module,exports){

},{}]},{},[207]);
