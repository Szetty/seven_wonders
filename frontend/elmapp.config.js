module.exports = {
    proxy: "http://192.168.1.64:8080",
    configureWebpack: (config, _env) => {
        // Object.defineProperty(RegExp.prototype, "toJSON", {
        //     value: RegExp.prototype.toString
        // });
        // console.log(JSON.stringify(config));
        config.module.rules.forEach((rule) => {
            if (!rule.test && rule.exclude) {
                rule.exclude.push(/\.ts$/)
            }
        });
        config.module.rules.unshift(
            {
                test:/\.(s*)css$/,
                use:[
                    {
                        loader: 'style-loader'
                    },
                    {
                        loader: 'css-loader',
                        options: {
                            sourceMap: true,
                        },
                    },
                    {
                        loader: 'sass-loader',
                        options: {
                            sourceMap: true,
                        },
                    }
                ]
            },
            {
                test: /\.ts$/,
                use: 'ts-loader',
                exclude: /node_modules/,
            },
        );
        config.resolve.extensions.push('.ts');
        config.devtool = 'inline-source-map';
        // console.log(JSON.stringify(config));
        return config;
    }
};