pragma solidity ^0.5.17;

contract Multifilter {

event E1(uint value1);
event E2(uint value2);

function fireE1(uint _value) public {
  emit E1(_value);
}

function fireE2(uint _value) public {
  emit E2(_value);
}

}
