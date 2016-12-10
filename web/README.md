# happy flowers Setup Guide

The following guide documents the steps to setting up the React environment for happy flowers to work in.

## Table of Contents

1. [Requirements](#requirements)
2. [yarn Tasks](#yarn-tasks)
3. [Atom Integration](#atom-integration)

## Requirements

| Technology | Version |
| ---------- | ------- |
| Node       | 7.2.0   |
| yarn       | 0.16.0  |

## yarn Tasks

- `yarn start` runs the app in development. A live-reloading `webpack-dev-server` is started on port 3000.
- `yarn run build` builds the optimised app for production.

Tasks and additional configuration are defined in [package.json](./src/package.json).

## Atom Integration

Atom offers easy integration of linting output directly in the editor.

1. Install [linter](https://atom.io/packages/linter)
2. Install [linter-eslint](https://atom.io/packages/linter-eslint)
3. Set up eslint [correctly](https://github.com/facebookincubator/create-react-app/blob/master/packages/react-scripts/template/README.md#displaying-lint-output-in-the-editor)
4. Install [linter-stylelint](https://atom.io/packages/linter-stylelint)
