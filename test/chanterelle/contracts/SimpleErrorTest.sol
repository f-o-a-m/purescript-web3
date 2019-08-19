pragma solidity ^0.4.21;

contract SimpleErrorTest {
    
    mapping(uint => string) public table;
    
    string[] public names;

    function testBool(bool _arg) view returns(bool){
      return !_arg;
    }

}
