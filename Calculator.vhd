-----------Package_BCD--------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
package BCD is
	function Segment (keypad : in integer range 0 to 13) 
	return std_logic_vector;
end package;

package body BCD is
	function Segment (keypad : in integer range 0 to 13)
	return std_logic_vector is
	variable ssd :std_logic_vector(6 downto 0);
	begin
		case keypad is
			when 0 => ssd:="0000001"; --"0" on SSD
			when 1 => ssd:="1001111"; --"1" on SSD
			when 2 => ssd:="0010010"; --"2" on SSD
			when 3 => ssd:="0000110"; --"3" on SSD
			when 4 => ssd:="1001100"; --"4" on SSD
			when 5 => ssd:="0100100"; --"5" on SSD
			when 6 => ssd:="0100000"; --"6" on SSD
			when 7 => ssd:="0001111"; --"7" on SSD
			when 8 => ssd:="0000000"; --"8" on SSD
			when 9 => ssd:="0000100"; --"9" on SSD
			when 10 => ssd:="0001000"; --"A" on SSD => ADD
			when 11 => ssd:="1100000"; --"b" on SSD => SUB
			when 12 => ssd:="0110000"; --"C" on SSD => MUL
			when 13 => ssd:="1000010"; --"d" on SSD => DIV

			when others => ssd:="0110000"; --"E"rror
		end case;
		return ssd;
	end function;
end package body;
							
----------MainCode---------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use work.BCD.all;

entity Calculator is

	port( keypad :in std_logic_vector(3 downto 0);
			output :out integer range 0 to 999999;
			input1, input2 :buffer integer range 0 to 99999;
			seg1,seg2,seg3,seg4,seg5,seg6,segS :out std_logic_vector(6 downto 0);
			S, M, A, D :out std_logic);
			
end Calculator;

architecture Behavioral of Calculator is
signal evn: std_logic := 'Z';
signal ca4,ca3,ca2,ca1,ca0,cb4,cb3,cb2,cb1,cb0 :integer range 0 to 9;
signal sign1,sign2,LED :std_logic_vector(3 downto 0):="0000";
signal store :integer range 0 to 9999;
signal sseg0,sseg1,sseg2,sseg3,sseg4 :integer range 0 to 9;

begin
evn <= keypad(0) AND keypad(1) AND keypad(2) AND keypad(3);
	save : process( evn,keypad,ca4,ca3,ca2,ca1,ca0,cb4,cb3,cb2,cb1,cb0,sign1,sign2 )
	type mem is array (4 downto 0) of integer range 0 to 9;
	variable c : mem;
	begin
		if(evn'event AND evn = '0')then
			if(keypad <= "1001" AND keypad >= "0000")then
				c(4) := c(3);
				sseg4 <= c(3);
				c(3) := c(2);
				sseg3 <= c(3);
            c(2) := c(1);
				sseg2 <= c(2);
            c(1) := c(0);
				sseg1 <= c(1);
            c(0) := conv_integer(Keypad);
				sseg0 <= c(0);
				sign2 <= "1111";
			elsif(keypad <= "1101" and  keypad >= "1010")then
				ca4 <= c(3);
				ca3 <= c(3);
				ca2 <= c(2);
				ca1 <= c(1);
				ca0 <= c(0);
				sign1 <= keypad;
				LED <= keypad;
				c(4):= 0;
				c(3):= 0;
				c(2):= 0;
				c(1):= 0;
				c(0):= 0;
			elsif(keypad ="1110")then
				cb4 <= c(4);
				cb3 <= c(3);
				cb2 <= c(2);
				cb1 <= c(1);
				cb0 <= c(0);
				sign2 <= keypad;
				LED <= "0000";
				c(4):= 0;
				c(3):= 0;
				c(2):= 0;
				c(1):= 0;
				c(0):= 0;			
         end if;
		end if;
		
	end process ; -- save
	
	input1 <= (ca4 *10000) + (ca3 *1000) + (ca2 *100) + (ca1 *10) + ca0;
	input2 <= (cb4 *10000) + (cb3 *1000) + (cb2 *100) + (cb1 *10) + cb0;
	store <= (input1 + input2) when sign1="1010" and sign2="1110" 
			else(input1 - input2) when sign1="1011" and sign2="1110"
			else(input1 * input2) when sign1="1100" and sign2="1110"
			else(input1 / input2) when sign1="1101" and sign2="1110"
			
			else 0 when sign1="1101" and sign2="1110" and (input2 > input1)
			else 0 when sign1="1101" and sign2="1110" and (input2 = 0)
			else 0;
			
	output <= store;
	seg1 <= Segment(store rem 10) when sign2="1110" 
		else Segment(sseg0);
	seg2 <= Segment((store rem 100)/10) when sign2="1110" 
		else Segment(sseg1);
	seg3 <= Segment((store rem 1000)/100) when sign2="1110" 
		else Segment(sseg2);
	seg4 <= Segment((store rem 10000)/1000) when sign2="1110" 
		else Segment(sseg3);
	seg5 <= Segment((store rem 100000)/10000) when sign2="1110" 
		else Segment(sseg4);
	seg6 <= Segment(store/100000) when sign2="1110" 
		else Segment(0);
		
	segS <= Segment(conv_integer(sign1)) when (sign2="1111" and (LED="1010" or  LED="1011" or LED="1100" or LED="1101"))
		else Segment(0);
		
	A <= '0' when (sign2="1110" and LED="1010") else '1' when (sign2="1111" and LED="1010") else '0';
	S <= '0' when (sign2="1110" and LED="1011") else '1' when (sign2="1111" and LED="1011") else '0';
	M <= '0' when (sign2="1110" and LED="1100") else '1' when (sign2="1111" and LED="1100") else '0';
	D <= '0' when (sign2="1110" and LED="1101") else '1' when (sign2="1111" and LED="1101") else '0';

end Behavioral;

