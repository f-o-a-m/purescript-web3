pragma solidity ^0.6.12;

contract MockERC20 {

    event Transfer(address indexed to, address indexed from, uint amount);

    function transfer(address to, uint amount) public {
        emit Transfer(to, msg.sender, amount);
    }
}
