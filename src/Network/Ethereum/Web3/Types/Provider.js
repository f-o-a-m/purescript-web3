"use strict";
import HttpProvider from "ethjs-provider-http";

export const metamaskProvider = function () {
    if (typeof web3 !== 'undefined') {
        return web3.currentProvider;
    } else {
        throw new Error("No Metamask provider found.");
    }
};

export const httpProvider = function (providerUrl) {
    return function () {
        if (typeof web3 !== 'undefined' && web3.currentProvider.host == providerUrl) {
           return web3.currentProvider;
        } else {
           return new HttpProvider(providerUrl);
        }
    };
};
