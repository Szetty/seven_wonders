module.exports = {
    proxy: "http://192.168.1.64:8080",
    configureWebpack: (config, _env) => {
        config.module.rules.push({
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
                },
            ]
        });
        return config;
    }
};