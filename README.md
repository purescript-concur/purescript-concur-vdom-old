<h1 align="center">
  Purescript Concur
</h1>
<p align="center">
   <img src="docs/logo.png" height="100">
</p>
<p align="center">
  <a href="https://gitter.im/concurhaskell" rel="nofollow">
      <img src="https://camo.githubusercontent.com/9fb4e2dde684214e7454d930a369f97190d1ecf2/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f6769747465722d6a6f696e253230636861742532302545322538362541332d626c75652e737667" alt="Join the chat at https://gitter.im/concurhaskell" data-canonical-src="https://img.shields.io/badge/gitter-join%20chat%20%E2%86%A3-blue.svg" style="max-width:100%;">
   </a>
   <a href="https://www.reddit.com/r/concurhaskell/" rel="nofollow">
      <img src="https://img.shields.io/badge/reddit-join%20the%20discussion%20%E2%86%A3-1158c2.svg" alt="Join the chat at https://gitter.im/concurhaskell" style="max-width:100%;">
   </a>
   <a href="https://pursuit.purescript.org/packages/purescript-concur-vdom">
     <img src="https://pursuit.purescript.org/packages/purescript-concur-vdom/badge"
        alt="Purescript-Concur-VDom on Pursuit">
     </img>
   </a>
</p>

[Concur UI Lib](https://github.com/ajnsit/concur) is a brand new client side Web UI framework that explores an entirely new paradigm. It does not follow FRP (think Reflex or Reactive Banana), or Elm architecture, but aims to combine the best parts of both. This repo contains the Concur implementation for Purescript, using the [Halogen-VDom](https://github.com/purescript-halogen/purescript-halogen-vdom) backend.

## Documentation

Work in progress tutorials are published in the [Concur Documentation site](https://github.com/ajnsit/concur-documentation/blob/master/README.md)

API documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-concur-vdom).

## Ports to other languages

Concur's model translates well to other platforms.

0. [Concur for Purescript (React Version)](https://github.com/purescript-concur/purescript-concur-react) - Purescript-Concur React Backend.
1. [Concur for Haskell](https://github.com/ajnsit/concur) - The original version of Concur written in Haskell.
2. [Concur for Javascript](https://github.com/ajnsit/concur-js) - An official but experimental port to Javascript.
3. [Concur for Python](https://github.com/potocpav/python-concur) - An unofficial and experimental port to Python. Uses ImgUI for graphics. Created and Maintained by [potocpav](https://github.com/potocpav).

## Installation

```bash
spago install purescript-concur-vdom
```

## Building from source

```bash
git clone https://github.com/purescript-concur/purescript-concur-vdom.git
cd purescript-concur-vdom
npm install
# Build library sources
npm run build
```
