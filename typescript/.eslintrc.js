module.exports = {
    "env": {
        "browser": true,
        "es2021": true
    },
    "extends": [
        "eslint:recommended",
        "plugin:@typescript-eslint/recommended"
    ],
    "overrides": [
        {
            "files": ["*.ts", "*.mts", "*.cts", "*.tsx"],
            "rules": {
                "@typescript-eslint/explicit-function-return-type": "error",
                "@typescript-eslint/explicit-module-boundary-types": "error",
                "@typescript-eslint/no-unused-vars": "off"
            }
        }
    ],
    "parser": "@typescript-eslint/parser",
    "parserOptions": {
        "ecmaVersion": "latest",
        "sourceType": "module"
    },
    "plugins": [
        "@typescript-eslint"
    ],
    "rules": {
    }
}
